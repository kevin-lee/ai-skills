package aiskills.cli.commands

import hedgehog.*
import hedgehog.runner.*

object UpdateSpec extends Properties {

  override def tests: List[Test] = List(
    // normalizeRepoUrl
    example("normalizeRepoUrl: normalizes HTTPS GitHub URL", testNormalizeHttps),
    example("normalizeRepoUrl: normalizes HTTPS GitHub URL with .git", testNormalizeHttpsDotGit),
    example("normalizeRepoUrl: normalizes SSH GitHub URL", testNormalizeSsh),
    example("normalizeRepoUrl: normalizes SSH GitHub URL without .git", testNormalizeSshNoDotGit),
    example("normalizeRepoUrl: HTTPS and SSH normalize to the same value", testHttpsSshSame),
    example("normalizeRepoUrl: normalizes HTTP URL", testNormalizeHttp),
    example("normalizeRepoUrl: normalizes git:// URL", testNormalizeGitProtocol),
    example("normalizeRepoUrl: normalizes non-GitHub host", testNormalizeGitLab),
    example("normalizeRepoUrl: strips trailing slash", testNormalizeTrailingSlash),
    example("normalizeRepoUrl: lowercases", testNormalizeLowercase),
    example("normalizeRepoUrl: handles unknown format", testNormalizeUnknown),
  )

  private def testNormalizeHttps: Result =
    Update.normalizeRepoUrl("https://github.com/owner/repo") ==== "github.com/owner/repo"

  private def testNormalizeHttpsDotGit: Result =
    Update.normalizeRepoUrl("https://github.com/owner/repo.git") ==== "github.com/owner/repo"

  private def testNormalizeSsh: Result =
    Update.normalizeRepoUrl("git@github.com:owner/repo.git") ==== "github.com/owner/repo"

  private def testNormalizeSshNoDotGit: Result =
    Update.normalizeRepoUrl("git@github.com:owner/repo") ==== "github.com/owner/repo"

  private def testHttpsSshSame: Result = {
    val https = Update.normalizeRepoUrl("https://github.com/anthropics/skills")
    val ssh   = Update.normalizeRepoUrl("git@github.com:anthropics/skills.git")
    https ==== ssh
  }

  private def testNormalizeHttp: Result =
    Update.normalizeRepoUrl("http://github.com/owner/repo") ==== "github.com/owner/repo"

  private def testNormalizeGitProtocol: Result =
    Update.normalizeRepoUrl("git://github.com/owner/repo.git") ==== "github.com/owner/repo"

  private def testNormalizeGitLab: Result =
    Update.normalizeRepoUrl("https://gitlab.com/group/project") ==== "gitlab.com/group/project"

  private def testNormalizeTrailingSlash: Result =
    Update.normalizeRepoUrl("https://github.com/owner/repo/") ==== "github.com/owner/repo"

  private def testNormalizeLowercase: Result =
    Update.normalizeRepoUrl("https://github.com/Owner/Repo") ==== "github.com/owner/repo"

  private def testNormalizeUnknown: Result =
    Update.normalizeRepoUrl("some-custom-url") ==== "some-custom-url"
}
