package aiskills.cli.commands

import aiskills.core.{Agent, GitBranch, InstallOptions, SkillLocation, SyncOptions}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object SharedSkillsDirIntegrationSpec extends Properties {

  override def tests: List[Test] = List(
    example("sync from project Universal to project Antigravity keeps the skill", testSyncSameDirKeepsSkill),
    example(
      "sync selected skill to Universal and Antigravity writes the shared dir one time",
      testSyncToSharedDirTargetsOnce
    ),
    example("install for Universal and Antigravity installs into the shared dir one time", testInstallSharedDirOnce),
    example(
      "install for Antigravity only shows the shared dir note and updates AGENTS.md",
      testInstallAntigravityOnly
    ),
    example("list collects a skill in the shared dir one time", testListSharedDirOnce),
  )

  private def withTemp(test: os.Path => Result): Result = {
    val dir = os.temp.dir(prefix = "aiskills-shared-dir-test-")
    try { test(dir) }
    finally { os.remove.all(dir) }
  }

  private def writeSkill(path: os.Path, name: String): Unit = {
    os.makeDir.all(path)
    os.write(path / "SKILL.md", s"---\nname: $name\ndescription: test\n---\n$name content\n")
  }

  private def captureOut(f: => Unit): String = {
    val buffer = new java.io.ByteArrayOutputStream()
    Console.withOut(buffer)(f)
    buffer.toString("UTF-8")
  }

  private def countOf(text: String, part: String): Int =
    text.split(java.util.regex.Pattern.quote(part), -1).length - 1

  private val antigravitySkippedLine =
    "Skipped: Antigravity (project) uses the same folder as Universal (.agents/skills)."

  private def installOptions(agents: List[Agent]): InstallOptions =
    InstallOptions(
      branch = none[GitBranch],
      locations = Set(SkillLocation.Project),
      agent = agents.some,
      yes = true,
    )

  private def testSyncSameDirKeepsSkill: Result = withTemp { dir =>
    val project = dir / "project"
    os.makeDir.all(project)
    val skillMd = project / ".agents" / "skills" / "kept-skill" / "SKILL.md"
    writeSkill(skillMd / os.up, "kept-skill")
    val before  = os.read(skillMd)

    val out = captureOut {
      os.dynamicPwd.withValue(project) {
        Sync.syncSkills(
          SyncOptions(
            skillNames = Nil,
            from = (SkillLocation.Project, Agent.Universal).some,
            to = List(Agent.Antigravity).some,
            targetLocations = Set(SkillLocation.Project),
            yes = true,
          )
        )
      }
    }

    Result.all(
      List(
        Result.assert(os.exists(skillMd)).log("Expected SKILL.md to be kept"),
        (if os.exists(skillMd) then os.read(skillMd) else "") ==== before,
        Result
          .assert(
            out.contains("Skipped: Universal (project) and Antigravity (project) use the same folder (.agents/skills).")
          )
          .log(s"Output:\n$out"),
      )
    )
  }

  private def testSyncToSharedDirTargetsOnce: Result = withTemp { dir =>
    val project = dir / "project"
    os.makeDir.all(project)
    writeSkill(project / ".claude" / "skills" / "shared-skill", "shared-skill")

    val out = captureOut {
      os.dynamicPwd.withValue(project) {
        Sync.syncSkills(
          SyncOptions(
            skillNames = List("shared-skill"),
            from = (SkillLocation.Project, Agent.Claude).some,
            to = List(Agent.Universal, Agent.Antigravity).some,
            targetLocations = Set(SkillLocation.Project),
            yes = true,
          )
        )
      }
    }

    Result.all(
      List(
        Result.assert(os.exists(project / ".agents" / "skills" / "shared-skill" / "SKILL.md")),
        (countOf(out, antigravitySkippedLine) ==== 1).log(s"Output:\n$out"),
        countOf(out, "Synced: shared-skill") ==== 1,
      )
    )
  }

  private def testInstallSharedDirOnce: Result = withTemp { dir =>
    val source  = dir / "src" / "install-skill"
    writeSkill(source, "install-skill")
    val project = dir / "project"
    os.makeDir.all(project)

    val out = captureOut {
      os.dynamicPwd.withValue(project) {
        Install.installSkill(source.toString, installOptions(List(Agent.Universal, Agent.Antigravity)))
      }
    }

    val agentsMd = project / "AGENTS.md"
    Result.all(
      List(
        Result.assert(os.exists(project / ".agents" / "skills" / "install-skill" / "SKILL.md")),
        (countOf(out, antigravitySkippedLine) ==== 1).log(s"Output:\n$out"),
        Result.assert(os.exists(agentsMd)).log("Expected AGENTS.md to be created"),
        Result.assert(os.exists(agentsMd) && os.read(agentsMd).contains("<name>install-skill</name>")),
      )
    )
  }

  private def testInstallAntigravityOnly: Result = withTemp { dir =>
    val source  = dir / "src" / "antigravity-skill"
    writeSkill(source, "antigravity-skill")
    val project = dir / "project"
    os.makeDir.all(project)

    val out = captureOut {
      os.dynamicPwd.withValue(project) {
        Install.installSkill(source.toString, installOptions(List(Agent.Antigravity)))
      }
    }

    val agentsMd = project / "AGENTS.md"
    Result.all(
      List(
        Result.assert(os.exists(project / ".agents" / "skills" / "antigravity-skill" / "SKILL.md")),
        Result
          .assert(out.contains("Note: Antigravity project skills are in .agents/skills, the same folder as Universal."))
          .log(s"Output:\n$out"),
        Result.assert(os.exists(agentsMd) && os.read(agentsMd).contains("<name>antigravity-skill</name>")),
      )
    )
  }

  private def testListSharedDirOnce: Result = withTemp { dir =>
    val project = dir / "project"
    os.makeDir.all(project)
    writeSkill(project / ".agents" / "skills" / "listed-skill", "listed-skill")

    val skills = os.dynamicPwd.withValue(project) {
      ListCmd.collectSkills(Agent.all, List(SkillLocation.Project))
    }

    Result.all(
      List(
        skills.map(_.name) ==== List("listed-skill"),
        skills.map(_.agent) ==== List(Agent.Universal),
      )
    )
  }
}
