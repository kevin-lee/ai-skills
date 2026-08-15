package aiskills.cli.commands

import aiskills.cli.commands.GitClone.{CloneCapabilities, CloneStrategy, CredentialHelperMode}
import aiskills.core.GitAuthMethod
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object GitCloneSpec extends Properties {

  override def tests: List[Test] = List(
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
    ghAvailable = true,
    credentialHelperConfigured = true,
    interactiveAllowed = true,
  )

  private val noCaps = CloneCapabilities(
    ghAvailable = false,
    credentialHelperConfigured = false,
    interactiveAllowed = false,
  )

  private val gitHubHttps = "https://github.com/owner/repo"
  private val gitHubSsh   = "git@github.com:owner/repo.git"

  private val fullGitHubChain = List(
    CloneStrategy(GitAuthMethod.Anonymous, gitHubHttps, CredentialHelperMode.Disabled, allowTerminalPrompt = false),
    CloneStrategy(GitAuthMethod.Ssh, gitHubSsh, CredentialHelperMode.Default, allowTerminalPrompt = true),
    CloneStrategy(GitAuthMethod.Gh, gitHubHttps, CredentialHelperMode.GhCli, allowTerminalPrompt = false),
    CloneStrategy(
      GitAuthMethod.CredentialHelper,
      gitHubHttps,
      CredentialHelperMode.Default,
      allowTerminalPrompt = false
    ),
    CloneStrategy(GitAuthMethod.Interactive, gitHubHttps, CredentialHelperMode.Default, allowTerminalPrompt = true),
  )

  private def methodsOf(strategies: List[CloneStrategy]): List[GitAuthMethod] = strategies.map(_.method)

  private def testOwnerRepoHttps: Result =
    GitClone.gitHubOwnerRepo("https://github.com/owner/repo") ==== "owner/repo".some

  private def testOwnerRepoHttpsDotGit: Result =
    GitClone.gitHubOwnerRepo("https://github.com/owner/repo.git") ==== "owner/repo".some

  private def testOwnerRepoHttpsTrailingSlash: Result =
    GitClone.gitHubOwnerRepo("https://github.com/owner/repo/") ==== "owner/repo".some

  private def testOwnerRepoSsh: Result =
    GitClone.gitHubOwnerRepo("git@github.com:owner/repo.git") ==== "owner/repo".some

  private def testOwnerRepoGitProtocol: Result =
    GitClone.gitHubOwnerRepo("git://github.com/owner/repo.git") ==== "owner/repo".some

  private def testOwnerRepoNotGitHubHttps: Result =
    GitClone.gitHubOwnerRepo("https://gitlab.com/owner/repo") ==== none[String]

  private def testOwnerRepoNotGitHubSsh: Result =
    GitClone.gitHubOwnerRepo("git@gitlab.com:owner/repo.git") ==== none[String]

  private def testOwnerRepoExtraSegments: Result =
    GitClone.gitHubOwnerRepo("https://github.com/owner/repo/skills/demo") ==== none[String]

  private def testOwnerRepoSingleSegment: Result =
    GitClone.gitHubOwnerRepo("https://github.com/owner") ==== none[String]

  private def testGitHubHttpsUrl: Result =
    GitClone.gitHubHttpsUrl("owner/repo") ==== "https://github.com/owner/repo"

  private def testGitHubSshUrl: Result =
    GitClone.gitHubSshUrl("owner/repo") ==== "git@github.com:owner/repo.git"

  private def testChainFull: Result =
    GitClone.buildStrategies(gitHubHttps, allCaps, none[GitAuthMethod]) ==== fullGitHubChain

  private def testChainNoGh: Result =
    methodsOf(
      GitClone.buildStrategies(gitHubHttps, allCaps.copy(ghAvailable = false), none[GitAuthMethod])
    ) ==== List(
      GitAuthMethod.Anonymous,
      GitAuthMethod.Ssh,
      GitAuthMethod.CredentialHelper,
      GitAuthMethod.Interactive,
    )

  private def testChainNoHelper: Result =
    methodsOf(
      GitClone.buildStrategies(gitHubHttps, allCaps.copy(credentialHelperConfigured = false), none[GitAuthMethod])
    ) ==== List(GitAuthMethod.Anonymous, GitAuthMethod.Ssh, GitAuthMethod.Gh, GitAuthMethod.Interactive)

  private def testChainNoInteractive: Result =
    methodsOf(
      GitClone.buildStrategies(gitHubHttps, allCaps.copy(interactiveAllowed = false), none[GitAuthMethod])
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
      GitClone.buildStrategies(gitHubHttps, allCaps.copy(ghAvailable = false), GitAuthMethod.Gh.some)
    ) ==== List(
      GitAuthMethod.Anonymous,
      GitAuthMethod.Ssh,
      GitAuthMethod.CredentialHelper,
      GitAuthMethod.Interactive,
    )

  private def testPreferredInteractiveNeverMoves: Result =
    GitClone.buildStrategies(gitHubHttps, allCaps, GitAuthMethod.Interactive.some) ==== fullGitHubChain

  private def testNonGitHubSsh: Result =
    GitClone.buildStrategies("git@gitlab.com:owner/repo.git", allCaps, none[GitAuthMethod]) ==== List(
      CloneStrategy(
        GitAuthMethod.Ssh,
        "git@gitlab.com:owner/repo.git",
        CredentialHelperMode.Default,
        allowTerminalPrompt = true,
      )
    )

  private def testNonGitHubHttps: Result =
    methodsOf(
      GitClone.buildStrategies("https://gitlab.com/owner/repo", allCaps, none[GitAuthMethod])
    ) ==== List(GitAuthMethod.Anonymous, GitAuthMethod.CredentialHelper, GitAuthMethod.Interactive)

  private def testNonGitHubGitProtocol: Result =
    GitClone.buildStrategies("git://gitlab.com/owner/repo.git", allCaps, none[GitAuthMethod]) ==== List(
      CloneStrategy(
        GitAuthMethod.Anonymous,
        "git://gitlab.com/owner/repo.git",
        CredentialHelperMode.Disabled,
        allowTerminalPrompt = false,
      )
    )

  private def testLocalGitPath: Result =
    GitClone.buildStrategies("/tmp/local/repo.git", noCaps, none[GitAuthMethod]) ==== List(
      CloneStrategy(
        GitAuthMethod.Anonymous,
        "/tmp/local/repo.git",
        CredentialHelperMode.Disabled,
        allowTerminalPrompt = false,
      )
    )
}
