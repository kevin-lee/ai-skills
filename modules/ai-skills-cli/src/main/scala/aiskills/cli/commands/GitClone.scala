package aiskills.cli.commands

import aiskills.core.{GitAuthMethod, GitBranch, GitHubOwnerRepo, RepoUrl}
import cats.*
import cats.derived.*
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*
import aiskills.cli.CliSpinner
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

  final case class CloneRequest(
    repoUrl: RepoUrl,
    targetPath: os.Path,
    branch: Option[GitBranch],
    preferred: Option[GitAuthMethod],
    interactivity: Interactivity,
    texts: CloneTexts,
  )

  enum CloneError derives Eq, Show {
    case Failed(failure: CloneFailure)
    case MissingBranch(branch: GitBranch, strategy: CloneStrategy)
    case InvalidBranch(branch: GitBranch, detail: String)
  }

  private[commands] enum CloneAttemptError derives Eq, Show {
    case MissingBranch(branch: GitBranch)
    case Failed(detail: String)
  }

  private[commands] enum AttemptChainResult derives Eq, Show {
    case Succeeded(strategy: CloneStrategy)
    case MissingBranch(branch: GitBranch, strategy: CloneStrategy)
    case Exhausted(attempts: List[CloneAttempt])
  }

  def validateBranch(branch: GitBranch): Either[String, Unit] = {
    if (branch.value.isEmpty || branch.value.contains("@{")) {
      s"Invalid Git branch '${branch.value}'".asLeft
    } else {
      Try(
        os.proc("git", "check-ref-format", "--branch", branch.value)
          .call(stdout = os.Pipe, stderr = os.Pipe, check = false)
      ) match {
        case Success(result) =>
          Either.cond(result.exitCode === 0, (), s"Invalid Git branch '${branch.value}': ${result.err.text().trim}")
        case Failure(ex) => s"Could not execute Git to validate branch '${branch.value}': ${errorTextOf(ex)}".asLeft
      }
    }
  }

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

  private def gitConfigArgs(strategy: CloneStrategy): List[String] = strategy.credentialHelper match {
    case CredentialHelperMode.Disabled => List("-c", "credential.helper=")
    case CredentialHelperMode.Default => List.empty[String]
    case CredentialHelperMode.GhCli =>
      List("-c", "credential.helper=", "-c", "credential.helper=!gh auth git-credential")
  }

  private def gitEnvironment(strategy: CloneStrategy): Map[String, String] = strategy.terminalPrompt match {
    case TerminalPrompt.Allowed => Map.empty[String, String]
    case TerminalPrompt.Suppressed => Map("GIT_TERMINAL_PROMPT" -> "0")
  }

  private[commands] def classifyBranchProbe(
    branch: GitBranch,
    exitCode: Int,
    stdout: String,
    stderr: String,
  ): Either[CloneAttemptError, Unit] = {
    val reference    = s"refs/heads/${branch.value}"
    val records      = stdout.linesIterator.filter(_.nonEmpty).toList
    val validRecords = records.forall { line =>
      line.split("\t", -1).toList match {
        case objectId :: ref :: Nil =>
          objectId.nonEmpty && objectId.forall(c => c.isDigit || "abcdefABCDEF".contains(c)) && ref === reference
        case _ => false
      }
    }
    exitCode match {
      case 2 => CloneAttemptError.MissingBranch(branch).asLeft
      case 0 if records.nonEmpty && validRecords => ().asRight
      case 0 =>
        CloneAttemptError.Failed(s"Could not verify branch '${branch.value}' from Git's reference output").asLeft
      case other => CloneAttemptError.Failed(s"Branch lookup failed (exit $other): ${stderr.trim}").asLeft
    }
  }

  private def probeBranch(strategy: CloneStrategy, branch: GitBranch): Either[CloneAttemptError, Unit] = {
    Try(
      os.proc(
        "git",
        gitConfigArgs(strategy),
        "ls-remote",
        "--exit-code",
        "--refs",
        "--",
        strategy.url.value,
        s"refs/heads/${branch.value}"
      ).call(
        env = gitEnvironment(strategy),
        stdin = if (strategy.method === GitAuthMethod.Interactive) os.Inherit else os.Pipe,
        stdout = os.Pipe,
        stderr = os.Pipe,
        check = false
      )
    ).toEither
      .left
      .map(ex => CloneAttemptError.Failed(errorTextOf(ex)))
      .flatMap(result => classifyBranchProbe(branch, result.exitCode, result.out.text(), result.err.text()))
  }

  private[commands] def runGitClone(
    strategy: CloneStrategy,
    targetPath: os.Path,
    branch: Option[GitBranch],
  ): Either[CloneAttemptError, Unit] = {
    val probe = branch.fold(().asRight[CloneAttemptError])(probeBranch(strategy, _))
    probe.flatMap { _ =>
      val branchArgs = branch.toList.flatMap(b => List("--branch", b.value))
      val command    = os.proc(
        "git",
        gitConfigArgs(strategy),
        "clone",
        "--depth",
        "1",
        "--quiet",
        branchArgs,
        "--",
        strategy.url.value,
        targetPath
      )
      val attempted  = Try {
        val _ = if (strategy.method === GitAuthMethod.Interactive) {
          command.call(env = gitEnvironment(strategy), stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
        } else {
          command.call(env = gitEnvironment(strategy), stderr = os.Pipe)
        }
      }.toEither.left.map(ex => CloneAttemptError.Failed(errorTextOf(ex)))
      val verified   = attempted.flatMap { _ =>
        branch.fold(().asRight[CloneAttemptError]) { selected =>
          Try(
            os.proc("git", "symbolic-ref", "--quiet", "HEAD").call(cwd = targetPath, stdout = os.Pipe, stderr = os.Pipe)
          ).toEither
            .left
            .map(ex => CloneAttemptError.Failed(errorTextOf(ex)))
            .flatMap(result =>
              Either.cond(
                result.out.text().trim === s"refs/heads/${selected.value}",
                (),
                CloneAttemptError.Failed(s"Clone did not check out branch '${selected.value}'")
              )
            )
        }
      }
      verified.left.map { error =>
        if (os.exists(targetPath)) { os.remove.all(targetPath) }
        else { () }
        error
      }
    }
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
  private[commands] def attemptEach(
    strategies: List[CloneStrategy],
    targetPath: os.Path,
    branch: Option[GitBranch],
    attempts: List[CloneAttempt],
    runAttempt: (CloneStrategy, os.Path, Option[GitBranch]) => Either[CloneAttemptError, Unit],
  ): AttemptChainResult = strategies match {
    case Nil => AttemptChainResult.Exhausted(attempts.reverse)
    case strategy :: rest =>
      runAttempt(strategy, targetPath, branch) match {
        case Right(_) => AttemptChainResult.Succeeded(strategy)
        case Left(CloneAttemptError.MissingBranch(selected)) => AttemptChainResult.MissingBranch(selected, strategy)
        case Left(CloneAttemptError.Failed(detail)) =>
          attemptEach(
            rest,
            targetPath,
            branch,
            CloneAttempt(strategy.method, strategy.url, detail) :: attempts,
            runAttempt
          )
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
    branch: Option[GitBranch],
    preferred: Option[GitAuthMethod],
    interactivity: Interactivity,
  ): Either[CloneError, CloneSuccess] = {
    val validation = branch.fold(().asRight[CloneError]) { selected =>
      validateBranch(selected).left.map(detail => CloneError.InvalidBranch(selected, detail))
    }
    validation.flatMap { _ =>
      val isGitHub       = gitHubOwnerRepo(repoUrl).isDefined
      val helperEligible = isGitHub || isHttpUrl(repoUrl)
      val caps           = CloneCapabilities(
        ghCli = if (isGitHub) detectGhAvailable() else GhCliStatus.Unavailable,
        credentialHelper =
          if (helperEligible) detectCredentialHelperConfigured() else CredentialHelperStatus.NotConfigured,
        interactivity = interactivity match {
          case Interactivity.Allowed => if (isStdinTty) Interactivity.Allowed else Interactivity.NotAllowed
          case Interactivity.NotAllowed => Interactivity.NotAllowed
        },
      )
      val strategies     = buildStrategies(repoUrl, caps, preferred)
      attemptEach(
        strategies.filter(_.method =!= GitAuthMethod.Interactive),
        targetPath,
        branch,
        List.empty[CloneAttempt],
        runGitClone
      ) match {
        case AttemptChainResult.Succeeded(strategy) => CloneSuccess(canonicalUrlOf(repoUrl), strategy.method).asRight
        case AttemptChainResult.MissingBranch(selected, strategy) => CloneError.MissingBranch(selected, strategy).asLeft
        case AttemptChainResult.Exhausted(attempts) =>
          CloneError
            .Failed(CloneFailure(attempts, caps, strategies.find(_.method === GitAuthMethod.Interactive).map(_.url)))
            .asLeft
      }
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
  def cloneRepoWithUi(request: CloneRequest): Either[CloneError, CloneSuccess] = {
    val spinner = CliSpinner.createDefaultSideEffect(
      SpinnerConfig.default.withText(request.texts.cloning).withColor(Color.cyan).withIndent(2),
    )
    val _       = spinner.start()
    cloneWithFallback(
      request.repoUrl,
      request.targetPath,
      request.branch,
      request.preferred,
      request.interactivity
    ) match {
      case Right(success) =>
        val _ = spinner.succeed(request.texts.cloned.some)
        success.asRight
      case Left(error) =>
        val _ = spinner.fail(request.texts.failed.some)
        error match {
          case CloneError.MissingBranch(branch, _) =>
            println(s"Branch '${branch.value}' does not exist in ${request.repoUrl.value}".yellow)
            error.asLeft
          case CloneError.InvalidBranch(_, detail) =>
            println(detail.red)
            error.asLeft
          case CloneError.Failed(failure) =>
            printAttemptReport(request.repoUrl, failure)
            failure.interactiveUrl match {
              case None =>
                printFinalTip()
                error.asLeft
              case Some(httpsUrl) =>
                println("SSH, gh, and credential helper access all failed or are unavailable.".yellow)
                askTryUserPassword() match {
                  case InteractiveCloneChoice.Abort =>
                    printFinalTip()
                    error.asLeft
                  case InteractiveCloneChoice.Proceed =>
                    val strategy = CloneStrategy(
                      GitAuthMethod.Interactive,
                      httpsUrl,
                      CredentialHelperMode.Default,
                      TerminalPrompt.Allowed
                    )
                    runGitClone(strategy, request.targetPath, request.branch) match {
                      case Right(_) =>
                        println(request.texts.cloned.green)
                        CloneSuccess(canonicalUrlOf(request.repoUrl), GitAuthMethod.Interactive).asRight
                      case Left(CloneAttemptError.MissingBranch(branch)) =>
                        println(s"Branch '${branch.value}' does not exist in ${request.repoUrl.value}".yellow)
                        CloneError.MissingBranch(branch, strategy).asLeft
                      case Left(CloneAttemptError.Failed(detail)) =>
                        printFinalTip()
                        CloneError
                          .Failed(
                            failure
                              .copy(attempts = failure.attempts :+ CloneAttempt(strategy.method, strategy.url, detail))
                          )
                          .asLeft
                    }
                }
            }
        }
    }
  }
}
