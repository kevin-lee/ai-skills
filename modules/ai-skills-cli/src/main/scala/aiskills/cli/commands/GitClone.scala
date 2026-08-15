package aiskills.cli.commands

import aiskills.core.{GitAuthMethod, GitHubOwnerRepo, RepoUrl}
import cats.*
import cats.derived.*
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*
import just.spinner.*

import scala.annotation.tailrec
import scala.scalanative.posix.unistd
import scala.util.{Failure, Success, Try}

/** Clone a Git repository, escalating through the auth methods that are actually available.
  *
  * For `github.com` the chain is anonymous https, ssh, `gh`, https with credential helpers,
  * and finally - only on a terminal - https with git's own username/password prompt.
  * Credentials are never read or stored by this CLI: the last step hands the terminal to git.
  */
object GitClone {

  /** Extract `owner/repo` from a github.com URL in https, ssh, or git:// form. */
  def gitHubOwnerRepo(url: RepoUrl): Option[GitHubOwnerRepo] = {
    val trimmed = url.value.trim
    val path    =
      if trimmed.startsWith("https://github.com/") then trimmed.stripPrefix("https://github.com/").some
      else if trimmed.startsWith("git@github.com:") then trimmed.stripPrefix("git@github.com:").some
      else if trimmed.startsWith("git://github.com/") then trimmed.stripPrefix("git://github.com/").some
      else none[String]

    path.flatMap { p =>
      p.stripSuffix("/").stripSuffix(".git").split("/").toList match {
        case owner :: repo :: Nil if owner.nonEmpty && repo.nonEmpty =>
          GitHubOwnerRepo.from(s"$owner/$repo").toOption
        case _ => none[GitHubOwnerRepo]
      }
    }
  }

  /** Canonical stored form for a github.com repo. */
  def gitHubHttpsUrl(ownerRepo: GitHubOwnerRepo): RepoUrl = RepoUrl(s"https://github.com/${ownerRepo.value}")

  def gitHubSshUrl(ownerRepo: GitHubOwnerRepo): RepoUrl = RepoUrl(s"git@github.com:${ownerRepo.value}.git")

  def isSshUrl(url: RepoUrl): Boolean = url.value.startsWith("git@") || url.value.startsWith("ssh://")

  def isHttpUrl(url: RepoUrl): Boolean = url.value.startsWith("https://") || url.value.startsWith("http://")

  /** How `credential.helper` is configured for a single clone attempt. */
  enum CredentialHelperMode derives Eq, Show {
    case Disabled, Default, GhCli
  }

  enum GhCliStatus derives Eq, Show {
    case Available, Unavailable
  }

  enum CredentialHelperStatus derives Eq, Show {
    case Configured, NotConfigured
  }

  enum Interactivity derives Eq, Show {
    case Allowed, NotAllowed
  }

  enum TerminalPrompt derives Eq, Show {
    case Allowed, Suppressed
  }

  enum InteractiveCloneChoice derives Eq, Show {
    case Proceed, Abort
  }

  final case class CloneStrategy(
    method: GitAuthMethod,
    url: RepoUrl,
    credentialHelper: CredentialHelperMode,
    terminalPrompt: TerminalPrompt,
  ) derives Eq,
        Show

  final case class CloneCapabilities(
    ghCli: GhCliStatus,
    credentialHelper: CredentialHelperStatus,
    interactivity: Interactivity,
  ) derives Eq,
        Show

  final case class CloneAttempt(method: GitAuthMethod, url: RepoUrl, error: String) derives Eq, Show

  final case class CloneSuccess(url: RepoUrl, method: GitAuthMethod) derives Eq, Show

  final case class CloneFailure(
    attempts: List[CloneAttempt],
    caps: CloneCapabilities,
    interactiveUrl: Option[RepoUrl],
  ) derives Eq,
        Show

  final case class CloneTexts(cloning: String, cloned: String, failed: String)

  /** Build the ordered list of attempts for a repo URL. Pure: capabilities are passed in. */
  def buildStrategies(
    repoUrl: RepoUrl,
    caps: CloneCapabilities,
    preferred: Option[GitAuthMethod],
  ): List[CloneStrategy] = {
    val base = gitHubOwnerRepo(repoUrl) match {
      case Some(ownerRepo) =>
        val https = gitHubHttpsUrl(ownerRepo)
        val ssh   = gitHubSshUrl(ownerRepo)
        List(
          CloneStrategy(
            GitAuthMethod.Anonymous,
            https,
            CredentialHelperMode.Disabled,
            TerminalPrompt.Suppressed,
          ).some,
          // ssh may legitimately prompt for a key passphrase
          CloneStrategy(GitAuthMethod.Ssh, ssh, CredentialHelperMode.Default, TerminalPrompt.Allowed).some,
          caps.ghCli match {
            case GhCliStatus.Available =>
              CloneStrategy(GitAuthMethod.Gh, https, CredentialHelperMode.GhCli, TerminalPrompt.Suppressed).some
            case GhCliStatus.Unavailable => none[CloneStrategy]
          },
          caps.credentialHelper match {
            case CredentialHelperStatus.Configured =>
              CloneStrategy(
                GitAuthMethod.CredentialHelper,
                https,
                CredentialHelperMode.Default,
                TerminalPrompt.Suppressed,
              ).some
            case CredentialHelperStatus.NotConfigured => none[CloneStrategy]
          },
          caps.interactivity match {
            case Interactivity.Allowed =>
              CloneStrategy(
                GitAuthMethod.Interactive,
                https,
                CredentialHelperMode.Default,
                TerminalPrompt.Allowed,
              ).some
            case Interactivity.NotAllowed => none[CloneStrategy]
          },
        ).flatten

      case None =>
        if isSshUrl(repoUrl) then List(
          CloneStrategy(GitAuthMethod.Ssh, repoUrl, CredentialHelperMode.Default, TerminalPrompt.Allowed)
        )
        else if isHttpUrl(repoUrl) then List(
          CloneStrategy(
            GitAuthMethod.Anonymous,
            repoUrl,
            CredentialHelperMode.Disabled,
            TerminalPrompt.Suppressed,
          ).some,
          caps.credentialHelper match {
            case CredentialHelperStatus.Configured =>
              CloneStrategy(
                GitAuthMethod.CredentialHelper,
                repoUrl,
                CredentialHelperMode.Default,
                TerminalPrompt.Suppressed,
              ).some
            case CredentialHelperStatus.NotConfigured => none[CloneStrategy]
          },
          caps.interactivity match {
            case Interactivity.Allowed =>
              CloneStrategy(
                GitAuthMethod.Interactive,
                repoUrl,
                CredentialHelperMode.Default,
                TerminalPrompt.Allowed,
              ).some
            case Interactivity.NotAllowed => none[CloneStrategy]
          },
        ).flatten
        else
          List(
            CloneStrategy(GitAuthMethod.Anonymous, repoUrl, CredentialHelperMode.Disabled, TerminalPrompt.Suppressed)
          )
    }

    moveToFront(base, preferred)
  }

  /** A recorded method is only a hint: it reorders the chain, never replaces it.
    * `Interactive` never moves - it must stay the last resort.
    */
  private def moveToFront(
    strategies: List[CloneStrategy],
    preferred: Option[GitAuthMethod],
  ): List[CloneStrategy] =
    preferred
      .filter(_ =!= GitAuthMethod.Interactive)
      .flatMap(method => strategies.find(_.method === method))
      .fold(strategies)(preferredStrategy =>
        preferredStrategy :: strategies.filterNot(_.method === preferredStrategy.method)
      )

  def isStdinTty: Boolean =
    Try(unistd.isatty(unistd.STDIN_FILENO) === 1).getOrElse(false)

  def detectGhAvailable(): GhCliStatus =
    if Try(os.proc("gh", "--version").call(stdout = os.Pipe, stderr = os.Pipe)).isSuccess
    then GhCliStatus.Available
    else GhCliStatus.Unavailable

  def detectCredentialHelperConfigured(): CredentialHelperStatus =
    if Try(
      os.proc("git", "config", "--get-regexp", "^credential(\\..+)?\\.helper$")
        .call(stdout = os.Pipe, stderr = os.Pipe)
    ).toOption.exists(_.out.text().trim.nonEmpty)
    then CredentialHelperStatus.Configured
    else CredentialHelperStatus.NotConfigured

  private def runGitClone(strategy: CloneStrategy, targetPath: os.Path): Try[Unit] = {
    val configArgs = strategy.credentialHelper match {
      case CredentialHelperMode.Disabled => List("-c", "credential.helper=")
      case CredentialHelperMode.Default => List.empty[String]
      // the empty value first clears any inherited helper, so gh is the only one asked
      case CredentialHelperMode.GhCli =>
        List("-c", "credential.helper=", "-c", "credential.helper=!gh auth git-credential")
    }

    val env = strategy.terminalPrompt match {
      case TerminalPrompt.Allowed => Map.empty[String, String]
      case TerminalPrompt.Suppressed => Map("GIT_TERMINAL_PROMPT" -> "0")
    }

    val command = os.proc("git", configArgs, "clone", "--depth", "1", "--quiet", strategy.url.value, targetPath)

    val attempted =
      if strategy.method === GitAuthMethod.Interactive then Try(
        command.call(env = env, stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
      )
      else
        Try(command.call(env = env, stderr = os.Pipe))

    attempted.transform(
      _ => Success(()),
      ex => {
        if os.exists(targetPath) then os.remove.all(targetPath) else ()
        Failure(ex)
      },
    )
  }

  /** Last non-empty line of the subprocess stderr, falling back to the exception message. */
  private def errorTextOf(ex: Throwable): String = {
    val stderrText = ex match {
      case subprocessFailure: os.SubprocessException => Try(subprocessFailure.result.err.text()).getOrElse("")
      case _ => ""
    }
    stderrText
      .linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
      .lastOption
      .orElse(Option(ex.getMessage).map(_.trim).filter(_.nonEmpty))
      .getOrElse("clone failed")
  }

  @tailrec
  private def attemptEach(
    strategies: List[CloneStrategy],
    targetPath: os.Path,
    attempts: List[CloneAttempt],
  ): Either[List[CloneAttempt], CloneStrategy] = strategies match {
    case Nil => attempts.reverse.asLeft
    case strategy :: rest =>
      runGitClone(strategy, targetPath) match {
        case Success(_) => strategy.asRight
        case Failure(ex) =>
          attemptEach(rest, targetPath, CloneAttempt(strategy.method, strategy.url, errorTextOf(ex)) :: attempts)
      }
  }

  private def canonicalUrlOf(repoUrl: RepoUrl): RepoUrl =
    gitHubOwnerRepo(repoUrl).fold(repoUrl)(gitHubHttpsUrl)

  /** Run the non-interactive part of the chain. The interactive last resort is left to
    * [[cloneRepoWithUi]] so this stays usable when no terminal is attached.
    */
  def cloneWithFallback(
    repoUrl: RepoUrl,
    targetPath: os.Path,
    preferred: Option[GitAuthMethod],
    interactivity: Interactivity,
  ): Either[CloneFailure, CloneSuccess] = {
    val isGitHub       = gitHubOwnerRepo(repoUrl).isDefined
    val helperEligible = isGitHub || isHttpUrl(repoUrl)

    val caps = CloneCapabilities(
      ghCli = if isGitHub then detectGhAvailable() else GhCliStatus.Unavailable,
      credentialHelper =
        if helperEligible then detectCredentialHelperConfigured() else CredentialHelperStatus.NotConfigured,
      interactivity = interactivity match {
        case Interactivity.Allowed => if isStdinTty then Interactivity.Allowed else Interactivity.NotAllowed
        case Interactivity.NotAllowed => Interactivity.NotAllowed
      },
    )

    val strategies = buildStrategies(repoUrl, caps, preferred)

    attemptEach(strategies.filter(_.method =!= GitAuthMethod.Interactive), targetPath, List.empty[CloneAttempt]) match {
      case Right(strategy) =>
        CloneSuccess(canonicalUrlOf(repoUrl), strategy.method).asRight

      case Left(attempts) =>
        CloneFailure(
          attempts,
          caps,
          strategies.find(_.method === GitAuthMethod.Interactive).map(_.url),
        ).asLeft
    }
  }

  private def printAttemptReport(repoUrl: RepoUrl, failure: CloneFailure): Unit = {
    failure.attempts.foreach { attempt =>
      println(s"  ${GitAuthMethod.render(attempt.method)}: ${attempt.error}".dim)
    }

    if gitHubOwnerRepo(repoUrl).isDefined then {
      failure.caps.ghCli match {
        case GhCliStatus.Unavailable =>
          println(s"  ${GitAuthMethod.render(GitAuthMethod.Gh)}: skipped (gh not found on PATH)".dim)
        case GhCliStatus.Available => ()
      }

      failure.caps.credentialHelper match {
        case CredentialHelperStatus.NotConfigured =>
          println(
            s"  ${GitAuthMethod.render(GitAuthMethod.CredentialHelper)}: skipped (no git credential helper configured)".dim
          )
        case CredentialHelperStatus.Configured => ()
      }
    } else ()
  }

  private def printFinalTip(): Unit =
    println(
      "Tip: For private repos, set up an SSH key, run `gh auth login`, or configure a git credential helper".yellow
    )

  private def askTryUserPassword(): InteractiveCloneChoice = {
    val options = List(
      "Yes          — git will prompt for username/password",
      "No           — abort",
    )

    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice(
        "Try https clone with username/password? (git will prompt, use a personal access token as the password)",
        options,
      ) match {
        case Completion.Finished(selected) =>
          if selected.startsWith("Yes") then InteractiveCloneChoice.Proceed else InteractiveCloneChoice.Abort
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          InteractiveCloneChoice.Abort
        case Completion.Fail(CompletionError.Error(_)) => InteractiveCloneChoice.Abort
      }
    }
  }

  /** Clone with a spinner, an explanation of every method that was tried, and - on a terminal -
    * the option to let git ask for a username/password as a last resort.
    */
  def cloneRepoWithUi(
    repoUrl: RepoUrl,
    targetPath: os.Path,
    preferred: Option[GitAuthMethod],
    interactivity: Interactivity,
    texts: CloneTexts,
  ): Either[CloneFailure, CloneSuccess] = {
    val spinner = Spinner.createDefaultSideEffect(
      SpinnerConfig
        .default
        .withText(texts.cloning)
        .withColor(Color.cyan)
        .withIndent(2),
    )
    val _       = spinner.start()

    cloneWithFallback(repoUrl, targetPath, preferred, interactivity) match {
      case Right(success) =>
        val _ = spinner.succeed(texts.cloned.some)
        success.asRight

      case Left(failure) =>
        val _ = spinner.fail(texts.failed.some)
        printAttemptReport(repoUrl, failure)

        failure.interactiveUrl match {
          case None =>
            printFinalTip()
            failure.asLeft

          case Some(httpsUrl) =>
            println("SSH, gh, and credential helper access all failed or are unavailable.".yellow)
            askTryUserPassword() match {
              case InteractiveCloneChoice.Abort =>
                printFinalTip()
                failure.asLeft

              case InteractiveCloneChoice.Proceed =>
                runGitClone(
                  CloneStrategy(
                    GitAuthMethod.Interactive,
                    httpsUrl,
                    CredentialHelperMode.Default,
                    TerminalPrompt.Allowed,
                  ),
                  targetPath,
                ) match {
                  case Success(_) =>
                    println(texts.cloned.green)
                    CloneSuccess(canonicalUrlOf(repoUrl), GitAuthMethod.Interactive).asRight

                  case Failure(ex) =>
                    printFinalTip()
                    failure
                      .copy(attempts =
                        failure.attempts :+ CloneAttempt(GitAuthMethod.Interactive, httpsUrl, errorTextOf(ex))
                      )
                      .asLeft
                }
            }
        }
    }
  }
}
