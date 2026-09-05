package aiskills.cli.commands

import aiskills.core.{*, given}
import aiskills.core.utils.{SkillMetadata, Yaml}
import cats.syntax.all.*
import GitClone.{CloneError, CloneRequest, CloneSuccess, Interactivity}
import Update.{GitUpdateError, ResolvedUpdateSource, SwitchBranchChoice, UpdateSourceError}
import scala.util.Try
import hedgehog.*
import hedgehog.runner.*

object UpdateSpec extends Properties {

  override def tests: List[Test] = List(
    example("groupGitSkills distinguishes branches and coalesces repository address forms", testBranchGrouping),
    example("successful source resolution preserves the selected branch", testSourceSuccess),
    example("missing branch can switch once with the working authentication method", testAcceptSwitch),
    example("declining a switch preserves the selection", testDeclineSwitch),
    example("noninteractive missing branch never prompts", testNoninteractiveSwitch),
    example("ordinary clone and invalid-branch failures never offer switching", testNoSwitchOnFailures),
    example("failed default clone does not clear the branch selection", testFallbackFailure),
    example("Git replacement preserves names and records the effective branch", testGitReplacement),
    example("preparation failure preserves the installed skill and metadata", testPreparationFailure),
    example("replacement failure restores the original installation", testReplacementRollback),
    example("rollback failure retains and identifies the recovery backup", testRollbackFailure),
    example("mixed repository group updates only skills present at their subpaths", testPartialGroup),
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
    Update.normalizeRepoUrl(RepoUrl("https://github.com/owner/repo")) ==== "github.com/owner/repo"

  private def testNormalizeHttpsDotGit: Result =
    Update.normalizeRepoUrl(RepoUrl("https://github.com/owner/repo.git")) ==== "github.com/owner/repo"

  private def testNormalizeSsh: Result =
    Update.normalizeRepoUrl(RepoUrl("git@github.com:owner/repo.git")) ==== "github.com/owner/repo"

  private def testNormalizeSshNoDotGit: Result =
    Update.normalizeRepoUrl(RepoUrl("git@github.com:owner/repo")) ==== "github.com/owner/repo"

  private def testHttpsSshSame: Result = {
    val https = Update.normalizeRepoUrl(RepoUrl("https://github.com/anthropics/skills"))
    val ssh   = Update.normalizeRepoUrl(RepoUrl("git@github.com:anthropics/skills.git"))
    https ==== ssh
  }

  private def testNormalizeHttp: Result =
    Update.normalizeRepoUrl(RepoUrl("http://github.com/owner/repo")) ==== "github.com/owner/repo"

  private def testNormalizeGitProtocol: Result =
    Update.normalizeRepoUrl(RepoUrl("git://github.com/owner/repo.git")) ==== "github.com/owner/repo"

  private def testNormalizeGitLab: Result =
    Update.normalizeRepoUrl(RepoUrl("https://gitlab.com/group/project")) ==== "gitlab.com/group/project"

  private def testNormalizeTrailingSlash: Result =
    Update.normalizeRepoUrl(RepoUrl("https://github.com/owner/repo/")) ==== "github.com/owner/repo"

  private def testNormalizeLowercase: Result =
    Update.normalizeRepoUrl(RepoUrl("https://github.com/Owner/Repo")) ==== "github.com/owner/repo"

  private def testNormalizeUnknown: Result =
    Update.normalizeRepoUrl(RepoUrl("some-custom-url")) ==== "some-custom-url"
  private val selectedBranch               = GitBranch("feature/New-Skill")
  private val repoUrl                      = RepoUrl("https://github.com/owner/repo")
  private val cloneSuccess                 = CloneSuccess(repoUrl, GitAuthMethod.Ssh)
  private val missingBranch                = CloneError.MissingBranch(
    selectedBranch,
    GitClone.CloneStrategy(
      GitAuthMethod.Ssh,
      RepoUrl("git@github.com:owner/repo.git"),
      GitClone.CredentialHelperMode.Default,
      GitClone.TerminalPrompt.Allowed
    )
  )
  private val cloneFailure                 = CloneError.Failed(
    GitClone.CloneFailure(
      Nil,
      GitClone.CloneCapabilities(
        GitClone.GhCliStatus.Unavailable,
        GitClone.CredentialHelperStatus.NotConfigured,
        Interactivity.NotAllowed
      ),
      none[RepoUrl]
    )
  )

  private def request(interactivity: Interactivity): CloneRequest = CloneRequest(
    repoUrl,
    os.pwd / "repo",
    selectedBranch.some,
    none[GitAuthMethod],
    interactivity,
    GitClone.CloneTexts("cloning", "cloned", "failed")
  )

  private def metadata(branch: Option[GitBranch], subpath: Option[String]): SkillSourceMetadata = {
    SkillSourceMetadata(
      name = "renamed".some,
      source = "owner/repo",
      sourceType = SkillSourceType.Git,
      repoUrl = repoUrl.some,
      branch = branch,
      authMethod = GitAuthMethod.Ssh.some,
      subpath = subpath,
      localPath = none[String],
      installedAt = "2026-09-05T12:53:00.000Z"
    )
  }

  private def testBranchGrouping: Result = {
    val skill    = Skill("demo", "", SkillLocation.Project, Agent.Claude, os.pwd / "demo")
    val selected = metadata(selectedBranch.some, "skills/demo".some)
    val inputs   = List(
      skill                            -> selected,
      skill.copy(agent = Agent.Cursor) -> selected.withRepoUrl(RepoUrl("git@github.com:owner/repo.git").some),
      skill                            -> selected.withBranch(GitBranch("feature/new-skill").some),
      skill                            -> selected.withBranch(GitBranch("main").some),
      skill                            -> selected.withBranch(none[GitBranch]),
    )
    val groups   = Update.groupGitSkills(inputs)
    Result.all(
      List(
        groups.size ==== 4,
        groups.get(Update.RepoBranchKey("github.com/owner/repo", selectedBranch.some)).map(_.size) ==== Some(2),
        groups.valuesIterator.map(_.size).sum ==== inputs.size,
      )
    )
  }

  private def testSourceSuccess: Result = {
    val calls    = List.newBuilder[String]
    val original = request(Interactivity.Allowed)
    val result   = Update.resolveUpdateSource(
      original,
      List("demo"),
      r => {
        calls += s"clone:${r.branch.map(_.value)}"
        cloneSuccess.asRight
      },
      (_, _, _) => {
        calls += "prompt"
        SwitchBranchChoice.UseDefaultBranch
      }
    )
    Result.all(
      List(
        result ==== Right(ResolvedUpdateSource(cloneSuccess, selectedBranch.some)),
        calls.result() ==== List(s"clone:${selectedBranch.some.map(_.value)}")
      )
    )
  }

  private def testAcceptSwitch: Result = {
    val calls    = List.newBuilder[String]
    val original = request(Interactivity.Allowed)
    val labels   = List("demo (Claude, project)", "demo (Cursor, global)")
    val result   = Update.resolveUpdateSource(
      original,
      labels,
      r => {
        r.branch match {
          case Some(_) =>
            calls += "branch clone"
            missingBranch.asLeft
          case None =>
            calls += s"default clone:${r.targetPath.last}:${r.preferred}"
            cloneSuccess.asRight
        }
      },
      (repo, branch, selected) => {
        calls += s"prompt:${repo.value}:${branch.value}:${selected.mkString(",")}"
        SwitchBranchChoice.UseDefaultBranch
      }
    )
    Result.all(
      List(
        result ==== Right(ResolvedUpdateSource(cloneSuccess, none[GitBranch])),
        calls.result() ==== List(
          "branch clone",
          s"prompt:${repoUrl.value}:${selectedBranch.value}:${labels.mkString(",")}",
          s"default clone:default-repo:${GitAuthMethod.Ssh.some}"
        )
      )
    )
  }

  private def testDeclineSwitch: Result = {
    val calls  = List.newBuilder[String]
    val result = Update.resolveUpdateSource(
      request(Interactivity.Allowed),
      Nil,
      _ => {
        calls += "clone"
        missingBranch.asLeft
      },
      (_, _, _) => {
        calls += "prompt"
        SwitchBranchChoice.KeepBranch
      }
    )
    Result.all(
      List(
        result ==== Left(UpdateSourceError.BranchRetained(selectedBranch)),
        calls.result() ==== List("clone", "prompt")
      )
    )
  }

  private def testNoninteractiveSwitch: Result = {
    val calls  = List.newBuilder[String]
    val result = Update.resolveUpdateSource(
      request(Interactivity.NotAllowed),
      Nil,
      _ => {
        calls += "clone"
        missingBranch.asLeft
      },
      (_, _, _) => {
        calls += "unexpected prompt"
        SwitchBranchChoice.UseDefaultBranch
      }
    )
    Result.all(
      List(result ==== Left(UpdateSourceError.BranchRetained(selectedBranch)), calls.result() ==== List("clone"))
    )
  }

  private def testNoSwitchOnFailures: Result = {
    Result.all(List(cloneFailure, CloneError.InvalidBranch(GitBranch("bad branch"), "invalid")).map { error =>
      val calls  = List.newBuilder[String]
      val result = Update.resolveUpdateSource(
        request(Interactivity.Allowed),
        Nil,
        _ => error.asLeft,
        (_, _, _) => {
          calls += "unexpected prompt"
          SwitchBranchChoice.UseDefaultBranch
        }
      )
      Result.all(List(result ==== Left(UpdateSourceError.CloneFailed(error)), calls.result() ==== Nil))
    })
  }

  private def testFallbackFailure: Result = {
    val calls    = List.newBuilder[String]
    val original = request(Interactivity.Allowed)
    val result   = Update.resolveUpdateSource(
      original,
      Nil,
      r => {
        calls += r.branch.fold("default")(_.value)
        if (r.branch.isDefined) missingBranch.asLeft else cloneFailure.asLeft
      },
      (_, _, _) => {
        calls += "prompt"
        SwitchBranchChoice.UseDefaultBranch
      }
    )
    Result.all(
      List(
        result ==== Left(UpdateSourceError.CloneFailed(cloneFailure)),
        original.branch ==== selectedBranch.some,
        calls.result() ==== List(selectedBranch.value, "prompt", "default")
      )
    )
  }

  private def withTemp(test: os.Path => Result): Result = {
    val dir = os.temp.dir(prefix = "aiskills-update-test-")
    try { test(dir) }
    finally { os.remove.all(dir) }
  }

  private def writeSkill(path: os.Path, body: String): Unit = {
    os.makeDir.all(path)
    os.write(path / "SKILL.md", s"---\nname: original\ndescription: test\n---\n$body\n")
  }

  private def testGitReplacement: Result = withTemp { dir =>
    val target      = dir / "installed"
    val source      = dir / "source"
    writeSkill(target, "old")
    writeSkill(source, "new")
    val original    = metadata(selectedBranch.some, "skills/demo".some)
    SkillMetadata.writeSkillMetadata(target, original)
    val regular     = Update.installGitUpdate(target, source, original)
    val regularMeta = SkillMetadata.readSkillMetadata(target)
    val switched    = original.withBranch(none[GitBranch])
    val result      = Update.installGitUpdate(target, source, switched)
    Result.all(
      List(
        regular ==== Right(()),
        regularMeta ==== Some(original),
        result ==== Right(()),
        SkillMetadata.readSkillMetadata(target) ==== Some(switched),
        Yaml.extractYamlField(os.read(target / "SKILL.md"), "name") ==== "renamed",
        Result.assert(os.read(target / "SKILL.md").contains("new")),
        Result.assert(!os.list(dir).exists(_.last.startsWith(".aiskills-update-"))),
      )
    )
  }

  private def testPreparationFailure: Result = withTemp { dir =>
    val target   = dir / "installed"
    writeSkill(target, "old")
    val original = metadata(selectedBranch.some, none[String])
    SkillMetadata.writeSkillMetadata(target, original)
    val before   = os.read(target / "SKILL.md")
    val result   = Update.installGitUpdate(target, dir / "missing-source", original.withBranch(none[GitBranch]))
    val isPreparationFailure = result match {
      case Left(GitUpdateError.PreparationFailed(_)) => true
      case Left(GitUpdateError.ReplacementFailed(_) | GitUpdateError.RollbackFailed(_, _)) | Right(_) => false
    }
    Result.all(
      List(
        Result.assert(isPreparationFailure),
        os.read(target / "SKILL.md") ==== before,
        SkillMetadata.readSkillMetadata(target) ==== Some(original)
      )
    )
  }

  private def testReplacementRollback: Result = withTemp { dir =>
    val target    = dir / "installed"
    val candidate = dir / "candidate"
    val backup    = dir / "backup"
    writeSkill(target, "old")
    writeSkill(candidate, "new")
    val original  = metadata(selectedBranch.some, none[String])
    SkillMetadata.writeSkillMetadata(target, original)
    val result    = Update.replaceGitUpdate(
      target,
      candidate,
      backup,
      (from, to) => {
        if (from === candidate) "injected replacement failure".asLeft
        else Try(os.move(from, to)).toEither.left.map(_.toString)
      }
    )
    Result.all(
      List(
        result ==== Left(GitUpdateError.ReplacementFailed("injected replacement failure")),
        SkillMetadata.readSkillMetadata(target) ==== Some(original),
        Result.assert(os.read(target / "SKILL.md").contains("old")),
        Result.assert(!os.exists(backup))
      )
    )
  }

  private def testRollbackFailure: Result = withTemp { dir =>
    val target    = dir / "installed"
    val candidate = dir / "candidate"
    val backup    = dir / "backup"
    writeSkill(target, "old")
    writeSkill(candidate, "new")
    val original  = metadata(selectedBranch.some, none[String])
    SkillMetadata.writeSkillMetadata(target, original)
    val result    = Update.replaceGitUpdate(
      target,
      candidate,
      backup,
      (from, to) => {
        if (from === target) Try(os.move(from, to)).toEither.left.map(_.toString)
        else "injected move failure".asLeft
      }
    )
    Result.all(
      List(
        result ==== Left(
          GitUpdateError.RollbackFailed(backup, "injected move failure. Rollback failed: injected move failure")
        ),
        SkillMetadata.readSkillMetadata(backup) ==== Some(original),
        Result.assert(os.read(backup / "SKILL.md").contains("old")),
      )
    )
  }

  private def testPartialGroup: Result = withTemp { dir =>
    val repo                          = dir / "remote"
    writeSkill(repo / "skills" / "present", "new default content")
    def git(args: List[String]): Unit = {
      val _ = os
        .proc(
          "git",
          "-c",
          "user.name=Branch Test",
          "-c",
          "user.email=branch-test@example.invalid",
          "-c",
          "commit.gpgsign=false",
          "-c",
          "core.hooksPath=/dev/null",
          args
        )
        .call(cwd = repo, stdout = os.Pipe, stderr = os.Pipe)
    }
    git(List("init", "--initial-branch=trunk"))
    git(List("add", "."))
    git(List("commit", "-m", "Default skills"))
    val project                       = dir / "project"
    val skillsDir                     = project / ".claude" / "skills"
    val presentName                   = s"${dir.last}-present"
    val missingName                   = s"${dir.last}-missing"
    val present                       = skillsDir / presentName
    val missing                       = skillsDir / missingName
    writeSkill(present, "old present")
    writeSkill(missing, "old missing")
    val presentMeta   = metadata(none[GitBranch], "skills/present".some).withRepoUrl(RepoUrl(s"file://$repo").some)
    val missingMeta   = metadata(none[GitBranch], "skills/missing".some).withRepoUrl(RepoUrl(s"file://$repo").some)
    SkillMetadata.writeSkillMetadata(present, presentMeta)
    SkillMetadata.writeSkillMetadata(missing, missingMeta)
    val missingBefore = os.read(missing / "SKILL.md")
    os.dynamicPwd.withValue(project) { Update.updateSkills(List(presentName, missingName)) }
    Result.all(
      List(
        Result.assert(os.read(present / "SKILL.md").contains("new default content")),
        SkillMetadata.readSkillMetadata(present).flatMap(_.branch) ==== none[GitBranch],
        os.read(missing / "SKILL.md") ==== missingBefore,
        SkillMetadata.readSkillMetadata(missing) ==== Some(missingMeta),
      )
    )
  }

}
