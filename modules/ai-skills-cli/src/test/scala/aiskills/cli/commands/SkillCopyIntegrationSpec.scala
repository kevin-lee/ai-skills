package aiskills.cli.commands

import aiskills.core.{Agent, GitBranch, InstallOptions, SkillLocation, SyncOptions}
import aiskills.core.utils.{SkillHash, SkillMetadata}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object SkillCopyIntegrationSpec extends Properties {

  override def tests: List[Test] = List(
    example("installing a repo-root skill from a Git source leaves out the clone's .git", testInstallGitRoot),
    example("installing a local skill folder that is a Git working tree leaves out its .git", testInstallLocalGit),
    example("syncing a skill leaves out a .git copied by an older install", testSyncWithoutGit),
  )

  private def withTemp(test: os.Path => Result): Result = {
    val dir = os.temp.dir(prefix = "aiskills-copy-test-")
    try { test(dir) }
    finally { os.remove.all(dir) }
  }

  private def git(cwd: os.Path, args: List[String]): Unit = {
    val _ = os
      .proc(
        "git",
        "-c",
        "user.name=Copy Test",
        "-c",
        "user.email=copy-test@example.invalid",
        "-c",
        "commit.gpgsign=false",
        "-c",
        "core.hooksPath=/dev/null",
        args
      )
      .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe)
  }

  private def writeSkill(path: os.Path, name: String): Unit = {
    os.makeDir.all(path)
    os.write(path / "SKILL.md", s"---\nname: $name\ndescription: test\n---\n$name content\n")
  }

  private val installOptions: InstallOptions = InstallOptions(
    branch = none[GitBranch],
    locations = Set(SkillLocation.Project),
    agent = List(Agent.Claude).some,
    yes = true,
  )

  private def testInstallGitRoot: Result = withTemp { dir =>
    val origin    = dir / "root-skill"
    writeSkill(origin, "root-skill")
    git(origin, List("init"))
    git(origin, List("add", "."))
    git(origin, List("commit", "-m", "Skill"))
    git(dir, List("clone", "--quiet", "--bare", origin.toString, (dir / "root-skill.git").toString))
    val project   = dir / "project"
    os.makeDir(project)
    os.dynamicPwd.withValue(project) {
      Install.installSkill(s"file://${dir / "root-skill.git"}", installOptions)
    }
    val installed = project / ".claude" / "skills" / "root-skill"
    val meta      = SkillMetadata.readSkillMetadata(installed)

    Result.all(
      List(
        Result.assert(os.exists(installed / "SKILL.md")).log("Expected SKILL.md to be installed"),
        Result.assert(!os.exists(installed / ".git")).log("Expected no .git in the installed skill"),
        meta.flatMap(_.subpath) ==== none[String],
        meta.flatMap(_.installedHash) ==== SkillHash.directoryHash(installed).toOption,
      )
    )
  }

  private def testInstallLocalGit: Result = withTemp { dir =>
    val local     = dir / "local-skill"
    writeSkill(local, "local-skill")
    git(local, List("init"))
    git(local, List("add", "."))
    git(local, List("commit", "-m", "Skill"))
    val project   = dir / "project"
    os.makeDir(project)
    os.dynamicPwd.withValue(project) {
      Install.installSkill(local.toString, installOptions)
    }
    val installed = project / ".claude" / "skills" / "local-skill"

    Result.all(
      List(
        Result.assert(os.exists(installed / "SKILL.md")).log("Expected SKILL.md to be installed"),
        Result.assert(!os.exists(installed / ".git")).log("Expected no .git in the installed skill"),
        Result.assert(os.exists(local / ".git")).log("Expected the source .git to be kept"),
      )
    )
  }

  private def testSyncWithoutGit: Result = withTemp { dir =>
    val project = dir / "project"
    val source  = project / ".claude" / "skills" / "synced-skill"
    writeSkill(source, "synced-skill")
    // Stands for the .git an older install copied from the clone.
    os.write(source / ".git" / "HEAD", "ref: refs/heads/main\n", createFolders = true)
    os.dynamicPwd.withValue(project) {
      Sync.syncSkills(
        SyncOptions(
          skillNames = List("synced-skill"),
          from = (SkillLocation.Project, Agent.Claude).some,
          to = List(Agent.Cursor).some,
          targetLocations = Set(SkillLocation.Project),
          yes = true,
        )
      )
    }
    val synced  = project / ".cursor" / "skills" / "synced-skill"

    Result.all(
      List(
        Result.assert(os.exists(synced / "SKILL.md")).log("Expected SKILL.md to be synced"),
        Result.assert(!os.exists(synced / ".git")).log("Expected no .git in the synced skill"),
      )
    )
  }

}
