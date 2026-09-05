package aiskills.cli.commands

import aiskills.cli.commands.GitClone.{
  CloneCapabilities,
  CloneStrategy,
  CredentialHelperMode,
  CredentialHelperStatus,
  GhCliStatus,
  Interactivity,
  TerminalPrompt
}
import aiskills.core.{GitAuthMethod, GitBranch, GitHubOwnerRepo, RepoUrl}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object GitCloneSpec extends Properties {

  override def tests: List[Test] = List(
    example("validateBranch preserves valid names and rejects invalid input", testBranchValidation),
    example("branch probe distinguishes missing references from other failures", testBranchProbe),
    example("attemptEach preserves branches across ordinary authentication failures", testBranchAttemptChain),
    example("attemptEach stops immediately on a confirmed missing branch", testMissingBranchStopsChain),
    example("interactive attempt receives the original branch", testInteractiveBranch),
    // gitHubOwnerRepo
    example("gitHubOwnerRepo: extracts from HTTPS URL", testOwnerRepoHttps),
    example("gitHubOwnerRepo: extracts from HTTPS URL with .git", testOwnerRepoHttpsDotGit),
    example("gitHubOwnerRepo: extracts from HTTPS URL with trailing slash", testOwnerRepoHttpsTrailingSlash),
    example("gitHubOwnerRepo: extracts from SSH URL", testOwnerRepoSsh),
    example("gitHubOwnerRepo: extracts from git:// URL", testOwnerRepoGitProtocol),
    example("gitHubOwnerRepo: rejects non-GitHub HTTPS URL", testOwnerRepoNotGitHubHttps),
    example("gitHubOwnerRepo: rejects non-GitHub SSH URL", testOwnerRepoNotGitHubSsh),
    example("gitHubOwnerRepo: rejects extra path segments", testOwnerRepoExtraSegments),
    example("gitHubOwnerRepo: rejects a single segment", testOwnerRepoSingleSegment),
    // GitHubOwnerRepo(String)
    example("""GitHubOwnerRepo("owner/repo"): accepts owner/repo""", testGitHubOwnerRepoApplyValid),
    example("""GitHubOwnerRepo("invalid String"): rejects invalid value""", testGitHubOwnerRepoApplyInvalid),
    // GitHubOwnerRepo.from
    example("GitHubOwnerRepo.from: accepts owner/repo", testGitHubOwnerRepoFromValid),
    example("GitHubOwnerRepo.from: rejects invalid forms", testGitHubOwnerRepoFromInvalid),
    // URL rendering
    example("gitHubHttpsUrl: renders the canonical HTTPS URL", testGitHubHttpsUrl),
    example("gitHubSshUrl: renders the SSH URL", testGitHubSshUrl),
    // buildStrategies
    example("buildStrategies: GitHub URL with all capabilities builds the full chain", testChainFull),
    example("buildStrategies: no gh available drops the gh step", testChainNoGh),
    example("buildStrategies: no credential helper drops the credential-helper step", testChainNoHelper),
    example("buildStrategies: interactive not allowed drops the interactive step", testChainNoInteractive),
    example("buildStrategies: SSH-form GitHub URL builds the same chain as HTTPS", testChainSshInputSameAsHttps),
    example("buildStrategies: preferred ssh moves ssh to the front", testPreferredSsh),
    example("buildStrategies: preferred gh is ignored when gh is unavailable", testPreferredGhUnavailable),
    example("buildStrategies: preferred interactive never moves", testPreferredInteractiveNeverMoves),
    example("buildStrategies: non-GitHub SSH URL yields a single ssh attempt", testNonGitHubSsh),
    example("buildStrategies: non-GitHub HTTPS URL has no gh step", testNonGitHubHttps),
    example("buildStrategies: git:// non-GitHub URL yields a single anonymous attempt", testNonGitHubGitProtocol),
    example("buildStrategies: local .git path yields a single anonymous attempt", testLocalGitPath),
  )

  private val allCaps = CloneCapabilities(
    ghCli = GhCliStatus.Available,
    credentialHelper = CredentialHelperStatus.Configured,
    interactivity = Interactivity.Allowed,
  )

  private val noCaps = CloneCapabilities(
    ghCli = GhCliStatus.Unavailable,
    credentialHelper = CredentialHelperStatus.NotConfigured,
    interactivity = Interactivity.NotAllowed,
  )

  private val gitHubHttps = RepoUrl("https://github.com/owner/repo")
  private val gitHubSsh   = RepoUrl("git@github.com:owner/repo.git")

  private val fullGitHubChain = List(
    CloneStrategy(GitAuthMethod.Anonymous, gitHubHttps, CredentialHelperMode.Disabled, TerminalPrompt.Suppressed),
    CloneStrategy(GitAuthMethod.Ssh, gitHubSsh, CredentialHelperMode.Default, TerminalPrompt.Allowed),
    CloneStrategy(GitAuthMethod.Gh, gitHubHttps, CredentialHelperMode.GhCli, TerminalPrompt.Suppressed),
    CloneStrategy(
      GitAuthMethod.CredentialHelper,
      gitHubHttps,
      CredentialHelperMode.Default,
      TerminalPrompt.Suppressed
    ),
    CloneStrategy(GitAuthMethod.Interactive, gitHubHttps, CredentialHelperMode.Default, TerminalPrompt.Allowed),
  )

  private def methodsOf(strategies: List[CloneStrategy]): List[GitAuthMethod] = strategies.map(_.method)

  private def testOwnerRepoHttps: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("https://github.com/owner/repo")) ==== GitHubOwnerRepo
      .unsafeFrom("owner/repo")
      .some

  private def testOwnerRepoHttpsDotGit: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("https://github.com/owner/repo.git")) ==== GitHubOwnerRepo
      .unsafeFrom("owner/repo")
      .some

  private def testOwnerRepoHttpsTrailingSlash: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("https://github.com/owner/repo/")) ==== GitHubOwnerRepo
      .unsafeFrom("owner/repo")
      .some

  private def testOwnerRepoSsh: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("git@github.com:owner/repo.git")) ==== GitHubOwnerRepo
      .unsafeFrom("owner/repo")
      .some

  private def testOwnerRepoGitProtocol: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("git://github.com/owner/repo.git")) ==== GitHubOwnerRepo
      .unsafeFrom("owner/repo")
      .some

  private def testOwnerRepoNotGitHubHttps: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("https://gitlab.com/owner/repo")) ==== none[GitHubOwnerRepo]

  private def testOwnerRepoNotGitHubSsh: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("git@gitlab.com:owner/repo.git")) ==== none[GitHubOwnerRepo]

  private def testOwnerRepoExtraSegments: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("https://github.com/owner/repo/skills/demo")) ==== none[GitHubOwnerRepo]

  private def testOwnerRepoSingleSegment: Result =
    GitClone.gitHubOwnerRepo(RepoUrl("https://github.com/owner")) ==== none[GitHubOwnerRepo]

  private def testGitHubOwnerRepoApplyValid: Result =
    GitHubOwnerRepo("owner/repo").value ==== "owner/repo"

  private def testGitHubOwnerRepoApplyInvalid: Result = {
    import scala.compiletime.testing.typeCheckErrors

    val expected1 = s"""Invalid value: ["owner"]. It must be ${GitHubOwnerRepo.inlinedExpectedValue}."""
    val actual1   = typeCheckErrors(
      """
      GitHubOwnerRepo("owner")
      """
    ).map(_.message).mkString("\n")

    val expected2 = s"""Invalid value: ["owner/"]. It must be ${GitHubOwnerRepo.inlinedExpectedValue}."""
    val actual2   = typeCheckErrors(
      """
      GitHubOwnerRepo("owner/")
      """
    ).map(_.message).mkString("\n")

    val expected3 = s"""Invalid value: ["/repo"]. It must be ${GitHubOwnerRepo.inlinedExpectedValue}."""
    val actual3   = typeCheckErrors(
      """
      GitHubOwnerRepo("/repo")
      """
    ).map(_.message).mkString("\n")

    val expected4 = s"""Invalid value: ["owner/repo/extra"]. It must be ${GitHubOwnerRepo.inlinedExpectedValue}."""
    val actual4   = typeCheckErrors(
      """
      GitHubOwnerRepo("owner/repo/extra")
      """
    ).map(_.message).mkString("\n")

    val expected5 = s"""Invalid value: ["/"]. It must be ${GitHubOwnerRepo.inlinedExpectedValue}."""
    val actual5   = typeCheckErrors(
      """
      GitHubOwnerRepo("/")
      """
    ).map(_.message).mkString("\n")

    Result.all(
      List(
        actual1 ==== expected1,
        actual2 ==== expected2,
        actual3 ==== expected3,
        actual4 ==== expected4,
        actual5 ==== expected5,
      )
    )
  }

  private def testGitHubOwnerRepoFromValid: Result =
    GitHubOwnerRepo.from("owner/repo").map(_.value) ==== "owner/repo".asRight[String]

  private def testGitHubOwnerRepoFromInvalid: Result =
    Result.all(
      List("owner", "owner/", "/repo", "owner/repo/extra").map(invalid =>
        Result.assert(GitHubOwnerRepo.from(invalid).isLeft).log(s"Expected Left for: $invalid")
      )
    )

  private def testGitHubHttpsUrl: Result =
    GitClone.gitHubHttpsUrl(GitHubOwnerRepo.unsafeFrom("owner/repo")) ==== RepoUrl("https://github.com/owner/repo")

  private def testGitHubSshUrl: Result =
    GitClone.gitHubSshUrl(GitHubOwnerRepo.unsafeFrom("owner/repo")) ==== RepoUrl("git@github.com:owner/repo.git")

  private def testChainFull: Result =
    GitClone.buildStrategies(gitHubHttps, allCaps, none[GitAuthMethod]) ==== fullGitHubChain

  private def testChainNoGh: Result =
    methodsOf(
      GitClone.buildStrategies(gitHubHttps, allCaps.copy(ghCli = GhCliStatus.Unavailable), none[GitAuthMethod])
    ) ==== List(
      GitAuthMethod.Anonymous,
      GitAuthMethod.Ssh,
      GitAuthMethod.CredentialHelper,
      GitAuthMethod.Interactive,
    )

  private def testChainNoHelper: Result =
    methodsOf(
      GitClone.buildStrategies(
        gitHubHttps,
        allCaps.copy(credentialHelper = CredentialHelperStatus.NotConfigured),
        none[GitAuthMethod],
      )
    ) ==== List(GitAuthMethod.Anonymous, GitAuthMethod.Ssh, GitAuthMethod.Gh, GitAuthMethod.Interactive)

  private def testChainNoInteractive: Result =
    methodsOf(
      GitClone.buildStrategies(gitHubHttps, allCaps.copy(interactivity = Interactivity.NotAllowed), none[GitAuthMethod])
    ) ==== List(
      GitAuthMethod.Anonymous,
      GitAuthMethod.Ssh,
      GitAuthMethod.Gh,
      GitAuthMethod.CredentialHelper,
    )

  private def testChainSshInputSameAsHttps: Result =
    GitClone.buildStrategies(gitHubSsh, allCaps, none[GitAuthMethod]) ==== fullGitHubChain

  private def testPreferredSsh: Result =
    methodsOf(GitClone.buildStrategies(gitHubHttps, allCaps, GitAuthMethod.Ssh.some)) ==== List(
      GitAuthMethod.Ssh,
      GitAuthMethod.Anonymous,
      GitAuthMethod.Gh,
      GitAuthMethod.CredentialHelper,
      GitAuthMethod.Interactive,
    )

  private def testPreferredGhUnavailable: Result =
    methodsOf(
      GitClone.buildStrategies(gitHubHttps, allCaps.copy(ghCli = GhCliStatus.Unavailable), GitAuthMethod.Gh.some)
    ) ==== List(
      GitAuthMethod.Anonymous,
      GitAuthMethod.Ssh,
      GitAuthMethod.CredentialHelper,
      GitAuthMethod.Interactive,
    )

  private def testPreferredInteractiveNeverMoves: Result =
    GitClone.buildStrategies(gitHubHttps, allCaps, GitAuthMethod.Interactive.some) ==== fullGitHubChain

  private def testNonGitHubSsh: Result =
    GitClone.buildStrategies(RepoUrl("git@gitlab.com:owner/repo.git"), allCaps, none[GitAuthMethod]) ==== List(
      CloneStrategy(
        GitAuthMethod.Ssh,
        RepoUrl("git@gitlab.com:owner/repo.git"),
        CredentialHelperMode.Default,
        TerminalPrompt.Allowed,
      )
    )

  private def testNonGitHubHttps: Result =
    methodsOf(
      GitClone.buildStrategies(RepoUrl("https://gitlab.com/owner/repo"), allCaps, none[GitAuthMethod])
    ) ==== List(GitAuthMethod.Anonymous, GitAuthMethod.CredentialHelper, GitAuthMethod.Interactive)

  private def testNonGitHubGitProtocol: Result =
    GitClone.buildStrategies(RepoUrl("git://gitlab.com/owner/repo.git"), allCaps, none[GitAuthMethod]) ==== List(
      CloneStrategy(
        GitAuthMethod.Anonymous,
        RepoUrl("git://gitlab.com/owner/repo.git"),
        CredentialHelperMode.Disabled,
        TerminalPrompt.Suppressed,
      )
    )

  private def testLocalGitPath: Result =
    GitClone.buildStrategies(RepoUrl("/tmp/local/repo.git"), noCaps, none[GitAuthMethod]) ==== List(
      CloneStrategy(
        GitAuthMethod.Anonymous,
        RepoUrl("/tmp/local/repo.git"),
        CredentialHelperMode.Disabled,
        TerminalPrompt.Suppressed,
      )
    )
  private val selectedBranch           = GitBranch("feature/New-Skill")

  private def testBranchValidation: Result = {
    Result.all(
      List("develop", "feature/New-Skill").map { value => GitClone.validateBranch(GitBranch(value)) ==== Right(()) } ++
        List(
          "",
          " ",
          "bad branch",
          "-option",
          "--upload-pack=bad",
          "@{-1}",
          "bad..branch",
          "bad:branch",
          "bad*branch",
          "bad?branch",
          "bad[branch",
          "bad.lock",
          "/branch",
          "branch/",
          "branch//name",
          "HEAD"
        ).map { value =>
          Result.assert(GitClone.validateBranch(GitBranch(value)).isLeft).log(value)
        }
    )
  }

  private def testBranchProbe: Result = {
    import GitClone.CloneAttemptError
    val reference = s"${"a" * 40}\trefs/heads/${selectedBranch.value}\n"
    Result.all(
      List(
        GitClone.classifyBranchProbe(selectedBranch, 0, reference, "") ==== Right(()),
        GitClone.classifyBranchProbe(selectedBranch, 2, "", "") ==== Left(
          CloneAttemptError.MissingBranch(selectedBranch)
        ),
        Result.assert(
          GitClone
            .classifyBranchProbe(selectedBranch, 128, "", "authentication failed")
            .left
            .exists { case CloneAttemptError.Failed(_) => true; case CloneAttemptError.MissingBranch(_) => false }
        ),
        Result.assert(GitClone.classifyBranchProbe(selectedBranch, 0, "", "").isLeft),
        Result.assert(
          GitClone.classifyBranchProbe(selectedBranch, 0, reference.replace("refs/heads", "refs/tags"), "").isLeft
        ),
        Result.assert(GitClone.classifyBranchProbe(selectedBranch, 0, "malformed", "").isLeft),
      )
    )
  }

  private def testBranchAttemptChain: Result = {
    import GitClone.{AttemptChainResult, CloneAttemptError}
    val strategies = fullGitHubChain.take(2)
    val result     = GitClone.attemptEach(
      strategies,
      os.pwd,
      selectedBranch.some,
      Nil,
      (strategy, _, branch) => {
        if (branch =!= selectedBranch.some) CloneAttemptError.Failed("branch lost").asLeft
        else if (strategy.method === GitAuthMethod.Anonymous) CloneAttemptError.Failed("authentication failed").asLeft
        else ().asRight
      }
    )
    val exhausted  = GitClone.attemptEach(
      strategies,
      os.pwd,
      selectedBranch.some,
      Nil,
      (_, _, _) => CloneAttemptError.Failed("failure").asLeft
    )
    Result.all(
      List(
        result ==== AttemptChainResult.Succeeded(
          CloneStrategy(GitAuthMethod.Ssh, gitHubSsh, CredentialHelperMode.Default, TerminalPrompt.Allowed)
        ),
        exhausted ==== AttemptChainResult.Exhausted(
          strategies.map(s => GitClone.CloneAttempt(s.method, s.url, "failure"))
        ),
      )
    )
  }

  private def testMissingBranchStopsChain: Result = {
    import GitClone.{AttemptChainResult, CloneAttemptError}
    val first =
      CloneStrategy(GitAuthMethod.Anonymous, gitHubHttps, CredentialHelperMode.Disabled, TerminalPrompt.Suppressed)
    GitClone.attemptEach(
      fullGitHubChain,
      os.pwd,
      selectedBranch.some,
      Nil,
      (strategy, _, _) => {
        if (strategy.method === GitAuthMethod.Anonymous) CloneAttemptError.MissingBranch(selectedBranch).asLeft
        else ().asRight
      }
    ) ==== AttemptChainResult.MissingBranch(selectedBranch, first)
  }

  private def testInteractiveBranch: Result = {
    import GitClone.{AttemptChainResult, CloneAttemptError}
    val strategy =
      CloneStrategy(GitAuthMethod.Interactive, gitHubHttps, CredentialHelperMode.Default, TerminalPrompt.Allowed)
    GitClone.attemptEach(
      List(strategy),
      os.pwd,
      selectedBranch.some,
      Nil,
      (_, _, branch) => Either.cond(branch === selectedBranch.some, (), CloneAttemptError.Failed("branch lost"))
    ) ====
      AttemptChainResult.Succeeded(strategy)
  }

}
