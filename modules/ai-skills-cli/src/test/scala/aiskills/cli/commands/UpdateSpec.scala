package aiskills.cli.commands

import aiskills.core.{*, given}
import aiskills.core.utils.{SkillHash, SkillMetadata, Yaml}
import cats.syntax.all.*
import GitClone.{CloneError, CloneRequest, CloneSuccess, Interactivity}
import Update.{
  GitUpdateError,
  LocalState,
  ResolvedUpdateSource,
  ResultStatus,
  SourceState,
  SwitchBranchChoice,
  UpdateDecision,
  UpdateMode,
  UpdateSourceError
}
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
    example("decideUpdate follows the version and local-change table", testDecisionTable),
    example("sourceState and localState compare recorded and current hashes", testStates),
    example("versionChange describes forced, changed, unrecorded and unknown versions", testVersionChange),
    example("statusLabel pads every status to the same width", testStatusLabel),
    example("checkedAtFor records the time for global skills only", testCheckedAtFor),
    example("an unchanged source leaves an up-to-date skill untouched", testUnchangedSource),
    example("a commit to another path keeps a skill up to date", testCommitToOtherPath),
    example("local edits with an unchanged source are kept", testLocalEditUnchangedSource),
    example("local edits with a changed source are kept until forced", testLocalEditChangedSource),
    example("a local source is versioned like a Git source", testLocalSourceVersioning),
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
      commit = none[GitCommitHash],
      sourceHash = none[ContentHash],
      installedHash = none[ContentHash],
      installedAt = "2026-09-05T12:53:00.000Z",
      checkedAt = none[String],
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
    val target       = dir / "installed"
    val source       = dir / "source"
    writeSkill(target, "old")
    writeSkill(source, "new")
    val original     = metadata(selectedBranch.some, "skills/demo".some)
    SkillMetadata.writeSkillMetadata(target, original)
    val regular      = Update.installGitUpdate(target, source, original)
    val regularMeta  = SkillMetadata.readSkillMetadata(target)
    val regularHash  = SkillHash.directoryHash(target).toOption
    val switched     = original.withBranch(none[GitBranch])
    val result       = Update.installGitUpdate(target, source, switched)
    val switchedMeta = SkillMetadata.readSkillMetadata(target)
    Result.all(
      List(
        regular ==== Right(()),
        regularMeta.map(_.withInstalledHash(none[ContentHash])) ==== Some(original),
        Result.assert(regularHash.isDefined).log("Expected a directory hash"),
        regularMeta.flatMap(_.installedHash) ==== regularHash,
        result ==== Right(()),
        switchedMeta.map(_.withInstalledHash(none[ContentHash])) ==== Some(switched),
        switchedMeta.flatMap(_.installedHash) ==== SkillHash.directoryHash(target).toOption,
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

  private def git(cwd: os.Path, args: List[String]): Unit = {
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
      .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe)
  }

  private def commitAll(repo: os.Path, message: String): Unit = {
    git(repo, List("add", "."))
    git(repo, List("commit", "-m", message))
  }

  final private case class GitFixture(repo: os.Path, project: os.Path, skillsDir: os.Path)

  /** A `file://` remote with `skills/present` on its `trunk` branch, and a project to install into. */
  private def gitFixture(dir: os.Path): GitFixture = {
    val repo    = dir / "remote"
    writeSkill(repo / "skills" / "present", "new default content")
    git(repo, List("init", "--initial-branch=trunk"))
    commitAll(repo, "Default skills")
    val project = dir / "project"
    GitFixture(repo, project, project / ".claude" / "skills")
  }

  private def fileRemoteMetadata(repo: os.Path, subpath: String): SkillSourceMetadata =
    metadata(none[GitBranch], subpath.some).withRepoUrl(RepoUrl(s"file://$repo").some)

  private def runUpdate(project: os.Path, names: List[String], mode: UpdateMode): Unit =
    os.dynamicPwd.withValue(project) { Update.updateSkills(names, mode) }

  /** Install `skills/present` as a project skill with legacy metadata, then update it once to record versions. */
  private def installPresent(dir: os.Path, fixture: GitFixture): os.Path = {
    val installed = fixture.skillsDir / s"${dir.last}-present"
    writeSkill(installed, "old present")
    SkillMetadata.writeSkillMetadata(installed, fileRemoteMetadata(fixture.repo, "skills/present"))
    runUpdate(fixture.project, List(installed.last), UpdateMode.Normal)
    installed
  }

  private def testPartialGroup: Result = withTemp { dir =>
    val fixture       = gitFixture(dir)
    val presentName   = s"${dir.last}-present"
    val missingName   = s"${dir.last}-missing"
    val present       = fixture.skillsDir / presentName
    val missing       = fixture.skillsDir / missingName
    writeSkill(present, "old present")
    writeSkill(missing, "old missing")
    val presentMeta   = fileRemoteMetadata(fixture.repo, "skills/present")
    val missingMeta   = fileRemoteMetadata(fixture.repo, "skills/missing")
    SkillMetadata.writeSkillMetadata(present, presentMeta)
    SkillMetadata.writeSkillMetadata(missing, missingMeta)
    val missingBefore = os.read(missing / "SKILL.md")
    runUpdate(fixture.project, List(presentName, missingName), UpdateMode.Normal)
    Result.all(
      List(
        Result.assert(os.read(present / "SKILL.md").contains("new default content")),
        SkillMetadata.readSkillMetadata(present).flatMap(_.branch) ==== none[GitBranch],
        Result.assert(SkillMetadata.readSkillMetadata(present).flatMap(_.sourceHash).isDefined),
        os.read(missing / "SKILL.md") ==== missingBefore,
        SkillMetadata.readSkillMetadata(missing) ==== Some(missingMeta),
      )
    )
  }

  private val hashA = ContentHash("1234567890abcdef1234567890abcdef12345678")
  private val hashB = ContentHash("abcdef0123456789abcdef0123456789abcdef01")

  private def testDecisionTable: Result = {
    val normal = List(
      (SourceState.Unchanged, LocalState.Clean, UpdateDecision.UpToDate),
      (SourceState.Unchanged, LocalState.Unknown, UpdateDecision.UpToDate),
      (SourceState.Unchanged, LocalState.Modified, UpdateDecision.KeepLocalChanges(SourceState.Unchanged)),
      (SourceState.Changed, LocalState.Clean, UpdateDecision.Replace),
      (SourceState.Changed, LocalState.Unknown, UpdateDecision.Replace),
      (SourceState.Changed, LocalState.Modified, UpdateDecision.KeepLocalChanges(SourceState.Changed)),
      (SourceState.Unknown, LocalState.Clean, UpdateDecision.Replace),
      (SourceState.Unknown, LocalState.Unknown, UpdateDecision.Replace),
      (SourceState.Unknown, LocalState.Modified, UpdateDecision.KeepLocalChanges(SourceState.Unknown)),
    )
    Result.all(
      normal.map {
        case (source, local, expected) =>
          (Update.decideUpdate(UpdateMode.Normal, source, local) ==== expected).log(s"Normal, $source, $local")
      } ++ normal.map {
        case (source, local, _) =>
          (Update.decideUpdate(UpdateMode.Force, source, local) ==== UpdateDecision.Replace)
            .log(s"Force, $source, $local")
      }
    )
  }

  private def testStates: Result =
    Result.all(
      List(
        Update.sourceState(hashA.some, hashA.some) ==== SourceState.Unchanged,
        Update.sourceState(hashA.some, hashB.some) ==== SourceState.Changed,
        Update.sourceState(hashA.some, none[ContentHash]) ==== SourceState.Unknown,
        Update.sourceState(none[ContentHash], hashB.some) ==== SourceState.Unknown,
        Update.sourceState(none[ContentHash], none[ContentHash]) ==== SourceState.Unknown,
        Update.localState(hashA.some, hashA.some) ==== LocalState.Clean,
        Update.localState(hashA.some, hashB.some) ==== LocalState.Modified,
        Update.localState(hashA.some, none[ContentHash]) ==== LocalState.Unknown,
        Update.localState(none[ContentHash], hashB.some) ==== LocalState.Unknown,
        Update.localState(none[ContentHash], none[ContentHash]) ==== LocalState.Unknown,
      )
    )

  private def testVersionChange: Result =
    Result.all(
      List(
        Update.versionChange(hashA.some, hashA.some) ==== "forced",
        Update.versionChange(hashA.some, hashB.some) ==== "1234567 → abcdef0",
        Update.versionChange(none[ContentHash], hashB.some) ==== "unrecorded → abcdef0",
        Update.versionChange(hashA.some, none[ContentHash]) ==== "version unknown",
        Update.versionChange(none[ContentHash], none[ContentHash]) ==== "version unknown",
      )
    )

  private def testStatusLabel: Result =
    Result.all(
      List(
        Update.statusLabel(ResultStatus.Updated) ==== "✅ Updated:      ",
        Update.statusLabel(ResultStatus.UpToDate) ==== "🟩 Up to date:   ",
        Update.statusLabel(ResultStatus.LocalChanges) ==== "🟨 Local changes:",
        Update.statusLabel(ResultStatus.Skipped) ==== "🟥 Skipped:      ",
      )
    )

  private def testCheckedAtFor: Result = {
    val now = "2026-09-27T00:00:00.000Z"
    Result.all(
      List(
        Update.checkedAtFor(SkillLocation.Global, now) ==== now.some,
        Update.checkedAtFor(SkillLocation.Project, now) ==== none[String],
      )
    )
  }

  private def testUnchangedSource: Result = withTemp { dir =>
    val fixture   = gitFixture(dir)
    val installed = installPresent(dir, fixture)
    val m1        = SkillMetadata.readSkillMetadata(installed)
    val content1  = os.read(installed / "SKILL.md")
    val raw1      = os.read(installed / SkillMetadata.SkillMetadataFile)
    runUpdate(fixture.project, List(installed.last), UpdateMode.Normal)
    Result.all(
      List(
        Result.assert(m1.flatMap(_.sourceHash).isDefined).log(s"m1: $m1"),
        Result.assert(m1.flatMap(_.installedHash).isDefined).log(s"m1: $m1"),
        m1.flatMap(_.checkedAt) ==== none[String],
        os.read(installed / "SKILL.md") ==== content1,
        os.read(installed / SkillMetadata.SkillMetadataFile) ==== raw1,
      )
    )
  }

  private def testCommitToOtherPath: Result = withTemp { dir =>
    val fixture   = gitFixture(dir)
    val installed = installPresent(dir, fixture)
    val m1        = SkillMetadata.readSkillMetadata(installed)
    writeSkill(fixture.repo / "skills" / "other", "other content")
    commitAll(fixture.repo, "Add another skill")
    runUpdate(fixture.project, List(installed.last), UpdateMode.Normal)
    Result.all(
      List(
        Result.assert(m1.flatMap(_.sourceHash).isDefined).log(s"m1: $m1"),
        SkillMetadata.readSkillMetadata(installed) ==== m1,
      )
    )
  }

  private def testLocalEditUnchangedSource: Result = withTemp { dir =>
    val fixture   = gitFixture(dir)
    val installed = installPresent(dir, fixture)
    val m1        = SkillMetadata.readSkillMetadata(installed)
    os.write.append(installed / "SKILL.md", "local edit\n")
    runUpdate(fixture.project, List(installed.last), UpdateMode.Normal)
    Result.all(
      List(
        Result.assert(m1.flatMap(_.installedHash).isDefined).log(s"m1: $m1"),
        Result.assert(os.read(installed / "SKILL.md").contains("local edit")),
        SkillMetadata.readSkillMetadata(installed) ==== m1,
      )
    )
  }

  private def testLocalEditChangedSource: Result = withTemp { dir =>
    val fixture   = gitFixture(dir)
    val installed = installPresent(dir, fixture)
    val m1        = SkillMetadata.readSkillMetadata(installed)
    os.write.append(installed / "SKILL.md", "local edit\n")
    os.write.append(fixture.repo / "skills" / "present" / "SKILL.md", "upstream change\n")
    commitAll(fixture.repo, "Change the present skill")

    runUpdate(fixture.project, List(installed.last), UpdateMode.Normal)
    val normalContent = os.read(installed / "SKILL.md")
    val normalMeta    = SkillMetadata.readSkillMetadata(installed)

    runUpdate(fixture.project, List(installed.last), UpdateMode.Force)
    val forcedContent = os.read(installed / "SKILL.md")
    val forcedMeta    = SkillMetadata.readSkillMetadata(installed)

    Result.all(
      List(
        Result.assert(normalContent.contains("local edit")),
        normalMeta ==== m1,
        Result.assert(!forcedContent.contains("local edit")),
        Result.assert(forcedContent.contains("upstream change")),
        Result
          .assert(
            forcedMeta.flatMap(_.sourceHash).isDefined && forcedMeta.flatMap(_.sourceHash) =!= m1.flatMap(_.sourceHash)
          )
          .log(s"forced: $forcedMeta, m1: $m1"),
        forcedMeta.flatMap(_.installedHash) ==== SkillHash.directoryHash(installed).toOption,
      )
    )
  }

  private def testLocalSourceVersioning: Result = withTemp { dir =>
    val source    = dir / "local-source"
    writeSkill(source, "local content")
    val project   = dir / "project"
    val installed = project / ".claude" / "skills" / s"${dir.last}-local"
    writeSkill(installed, "old local")
    SkillMetadata.writeSkillMetadata(
      installed,
      SkillSourceMetadata(
        source = source.toString,
        sourceType = SkillSourceType.Local,
        repoUrl = none[RepoUrl],
        branch = none[GitBranch],
        authMethod = none[GitAuthMethod],
        subpath = none[String],
        localPath = source.toString.some,
        commit = none[GitCommitHash],
        sourceHash = none[ContentHash],
        installedHash = none[ContentHash],
        installedAt = "2026-09-05T12:53:00.000Z",
        checkedAt = none[String],
      )
    )

    val sourceHash1 = SkillHash.sourceDirectoryHash(source).toOption
    runUpdate(project, List(installed.last), UpdateMode.Normal)
    val m1          = SkillMetadata.readSkillMetadata(installed)
    runUpdate(project, List(installed.last), UpdateMode.Normal)
    val m2          = SkillMetadata.readSkillMetadata(installed)
    os.write.append(source / "SKILL.md", "source edit\n")
    runUpdate(project, List(installed.last), UpdateMode.Normal)
    val m3          = SkillMetadata.readSkillMetadata(installed)

    Result.all(
      List(
        Result.assert(sourceHash1.isDefined).log("Expected a source hash"),
        m1.flatMap(_.sourceHash) ==== sourceHash1,
        m2 ==== m1,
        Result.assert(os.read(installed / "SKILL.md").contains("source edit")),
        m3.flatMap(_.sourceHash) ==== SkillHash.sourceDirectoryHash(source).toOption,
      )
    )
  }

}
