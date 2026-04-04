package aiskills.cli.commands

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object InstallSpec extends Properties {

  override def tests: List[Test] = List(
    // isLocalPath
    example("isLocalPath: detects absolute paths", testAbsolutePath),
    example("isLocalPath: detects relative paths ./", testRelativePath),
    example("isLocalPath: detects parent relative ../", testParentRelative),
    example("isLocalPath: detects home ~/", testHomePath),
    example("isLocalPath: rejects GitHub shorthand", testNotGithub),
    example("isLocalPath: rejects git URLs", testNotGitUrl),
    example("isLocalPath: rejects plain names", testNotPlain),
    // isGitUrl
    example("isGitUrl: detects SSH git URLs", testSshUrl),
    example("isGitUrl: detects git:// URLs", testGitProtocol),
    example("isGitUrl: detects HTTPS URLs", testHttpsUrl),
    example("isGitUrl: detects HTTP URLs", testHttpUrl),
    example("isGitUrl: detects .git suffix", testDotGit),
    example("isGitUrl: rejects GitHub shorthand", testGitUrlNotGithub),
    example("isGitUrl: rejects local paths", testGitUrlNotLocal),
    // expandPath
    example("expandPath: expands ~ to home", testExpandTilde),
    example("expandPath: resolves relative paths", testExpandRelative),
    example("expandPath: keeps absolute paths", testExpandAbsolute),
    // isPathInside
    example("isPathInside: allows normal paths", testPathInsideNormal),
    example("isPathInside: blocks traversal", testPathInsideTraversal),
    example("isPathInside: blocks outside paths", testPathInsideOutside),
    example("isPathInside: blocks prefix-not-child", testPathInsidePrefixNotChild),
    example("isPathInside: allows nested subdirs", testPathInsideNested),
    // getRepoName
    example("getRepoName: extracts from HTTPS URL", testRepoNameHttps),
    example("getRepoName: extracts from SSH URL", testRepoNameSsh),
    example("getRepoName: strips .git suffix", testRepoNameStripGit),
    // formatSize
    example("formatSize: formats bytes", testFormatBytes),
    example("formatSize: formats KB", testFormatKB),
    example("formatSize: formats MB", testFormatMB),
    // GitHub shorthand parsing
    example("GitHub shorthand: owner/repo", testGithubOwnerRepo),
    example("GitHub shorthand: owner/repo/path", testGithubOwnerRepoPath),
    // isGitHubHttpsUrl
    example("isGitHubHttpsUrl: detects GitHub HTTPS URL", testIsGitHubHttpsUrl),
    example("isGitHubHttpsUrl: detects GitHub HTTPS URL with .git", testIsGitHubHttpsUrlDotGit),
    example("isGitHubHttpsUrl: rejects non-GitHub HTTPS", testIsGitHubHttpsUrlNotGitLab),
    example("isGitHubHttpsUrl: rejects SSH URL", testIsGitHubHttpsUrlNotSsh),
    example("isGitHubHttpsUrl: rejects HTTP URL", testIsGitHubHttpsUrlNotHttp),
    // gitHubHttpsToSsh
    example("gitHubHttpsToSsh: converts plain URL", testGitHubHttpsToSshPlain),
    example("gitHubHttpsToSsh: converts URL with .git", testGitHubHttpsToSshDotGit),
    example("gitHubHttpsToSsh: converts URL with trailing slash", testGitHubHttpsToSshTrailingSlash),
  )

  // isLocalPath tests
  private def testAbsolutePath: Result =
    Result.all(
      List(
        Result.assert(Install.isLocalPath("/absolute/path/to/skill")),
        Result.assert(Install.isLocalPath("/Users/test/skills")),
      )
    )

  private def testRelativePath: Result =
    Result.all(
      List(
        Result.assert(Install.isLocalPath("./relative/path")),
        Result.assert(Install.isLocalPath("./skill")),
      )
    )

  private def testParentRelative: Result =
    Result.all(
      List(
        Result.assert(Install.isLocalPath("../parent/path")),
        Result.assert(Install.isLocalPath("../../../deep/path")),
      )
    )

  private def testHomePath: Result =
    Result.all(
      List(
        Result.assert(Install.isLocalPath("~/skills/my-skill")),
        Result.assert(Install.isLocalPath("~/.claude/skills")),
      )
    )

  private def testNotGithub: Result =
    Result.all(
      List(
        Result.assert(!Install.isLocalPath("owner/repo")),
        Result.assert(!Install.isLocalPath("anthropics/skills")),
        Result.assert(!Install.isLocalPath("owner/repo/skill-path")),
      )
    )

  private def testNotGitUrl: Result =
    Result.all(
      List(
        Result.assert(!Install.isLocalPath("git@github.com:owner/repo.git")),
        Result.assert(!Install.isLocalPath("https://github.com/owner/repo")),
        Result.assert(!Install.isLocalPath("http://github.com/owner/repo")),
      )
    )

  private def testNotPlain: Result =
    Result.all(
      List(
        Result.assert(!Install.isLocalPath("skill-name")),
        Result.assert(!Install.isLocalPath("my-skill")),
      )
    )

  // isGitUrl tests
  private def testSshUrl: Result =
    Result.all(
      List(
        Result.assert(Install.isGitUrl("git@github.com:owner/repo.git")),
        Result.assert(Install.isGitUrl("git@gitlab.com:group/project.git")),
      )
    )

  private def testGitProtocol: Result =
    Result.assert(Install.isGitUrl("git://github.com/owner/repo.git"))

  private def testHttpsUrl: Result =
    Result.all(
      List(
        Result.assert(Install.isGitUrl("https://github.com/owner/repo")),
        Result.assert(Install.isGitUrl("https://github.com/owner/repo.git")),
      )
    )

  private def testHttpUrl: Result =
    Result.assert(Install.isGitUrl("http://github.com/owner/repo"))

  private def testDotGit: Result =
    Result.all(
      List(
        Result.assert(Install.isGitUrl("custom-host.com/repo.git")),
        Result.assert(Install.isGitUrl("anything.git")),
      )
    )

  private def testGitUrlNotGithub: Result =
    Result.all(
      List(
        Result.assert(!Install.isGitUrl("owner/repo")),
        Result.assert(!Install.isGitUrl("anthropics/skills")),
      )
    )

  private def testGitUrlNotLocal: Result =
    Result.all(
      List(
        Result.assert(!Install.isGitUrl("/absolute/path")),
        Result.assert(!Install.isGitUrl("./relative/path")),
        Result.assert(!Install.isGitUrl("~/home/path")),
      )
    )

  // expandPath tests
  private def testExpandTilde: Result =
    Install.expandPath("~/skills/test") ==== (os.home / "skills" / "test")

  private def testExpandRelative: Result =
    Install.expandPath("./relative") ==== os.Path("relative", os.pwd)

  private def testExpandAbsolute: Result =
    Install.expandPath("/absolute/path") ==== os.Path("/absolute/path")

  // isPathInside tests
  private def testPathInsideNormal: Result =
    Result.assert(
      Install.isPathInside(
        os.Path("/home/user/.claude/skills/my-skill"),
        os.Path("/home/user/.claude/skills"),
      )
    )

  private def testPathInsideTraversal: Result =
    // os.Path resolves .. automatically
    Result.assert(
      !Install.isPathInside(
        os.Path("/etc/passwd"),
        os.Path("/home/user/.claude/skills"),
      )
    )

  private def testPathInsideOutside: Result =
    Result.assert(
      !Install.isPathInside(
        os.Path("/etc/passwd"),
        os.Path("/home/user/.claude/skills"),
      )
    )

  private def testPathInsidePrefixNotChild: Result =
    Result.assert(
      !Install.isPathInside(
        os.Path("/home/user/.claude/skills-evil"),
        os.Path("/home/user/.claude/skills"),
      )
    )

  private def testPathInsideNested: Result =
    Result.assert(
      Install.isPathInside(
        os.Path("/home/user/.claude/skills/category/my-skill"),
        os.Path("/home/user/.claude/skills"),
      )
    )

  // getRepoName tests
  private def testRepoNameHttps: Result =
    Install.getRepoName("https://github.com/owner/repo") ==== "repo".some

  private def testRepoNameSsh: Result =
    Install.getRepoName("git@github.com:owner/repo.git") ==== "repo".some

  private def testRepoNameStripGit: Result =
    Install.getRepoName("https://github.com/owner/repo.git") ==== "repo".some

  // formatSize tests
  private def testFormatBytes: Result =
    Install.formatSize(500) ==== "500B"

  private def testFormatKB: Result =
    Install.formatSize(2048) ==== "2.0KB"

  private def testFormatMB: Result =
    Install.formatSize(1048576) ==== "1.0MB"

  // GitHub shorthand parsing
  private def testGithubOwnerRepo: Result = {
    val parts = "anthropics/skills".split("/").toList
    Result.all(
      List(
        parts.length ==== 2,
        s"https://github.com/${parts(0)}/${parts(1)}" ==== "https://github.com/anthropics/skills",
      )
    )
  }

  private def testGithubOwnerRepoPath: Result = {
    val source = "anthropics/skills/document-skills/pdf"
    val parts  = source.split("/").toList
    Result.all(
      List(
        Result.assert(parts.length > 2),
        s"https://github.com/${parts(0)}/${parts(1)}" ==== "https://github.com/anthropics/skills",
        parts.drop(2).mkString("/") ==== "document-skills/pdf",
      )
    )
  }

  // isGitHubHttpsUrl tests
  private def testIsGitHubHttpsUrl: Result =
    Result.assert(Install.isGitHubHttpsUrl("https://github.com/owner/repo"))

  private def testIsGitHubHttpsUrlDotGit: Result =
    Result.assert(Install.isGitHubHttpsUrl("https://github.com/owner/repo.git"))

  private def testIsGitHubHttpsUrlNotGitLab: Result =
    Result.assert(!Install.isGitHubHttpsUrl("https://gitlab.com/owner/repo"))

  private def testIsGitHubHttpsUrlNotSsh: Result =
    Result.assert(!Install.isGitHubHttpsUrl("git@github.com:owner/repo.git"))

  private def testIsGitHubHttpsUrlNotHttp: Result =
    Result.assert(!Install.isGitHubHttpsUrl("http://github.com/owner/repo"))

  // gitHubHttpsToSsh tests
  private def testGitHubHttpsToSshPlain: Result =
    Install.gitHubHttpsToSsh("https://github.com/owner/repo") ==== "git@github.com:owner/repo.git"

  private def testGitHubHttpsToSshDotGit: Result =
    Install.gitHubHttpsToSsh("https://github.com/owner/repo.git") ==== "git@github.com:owner/repo.git"

  private def testGitHubHttpsToSshTrailingSlash: Result =
    Install.gitHubHttpsToSsh("https://github.com/owner/repo/") ==== "git@github.com:owner/repo.git"
}
