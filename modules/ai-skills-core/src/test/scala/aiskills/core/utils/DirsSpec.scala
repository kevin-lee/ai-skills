package aiskills.core.utils

import aiskills.core.{Agent, SkillLocation}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object DirsSpec extends Properties {

  override def tests: List[Test] = List(
    example("getSkillsDir: project Universal uses .agents", testProjectUniversal),
    example("getSkillsDir: global Universal uses .agents", testGlobalUniversal),
    example("getSkillsDir: project Claude uses .claude", testProjectClaude),
    example("getSkillsDir: global Claude uses .claude", testGlobalClaude),
    example("getSkillsDir: project Copilot uses .github", testProjectCopilot),
    example("getSkillsDir: global Copilot uses .copilot (asymmetric)", testGlobalCopilot),
    example("getSkillsDir: project Cursor uses .cursor", testProjectCursor),
    example("getSkillsDir: project Codex uses .codex", testProjectCodex),
    example("getSkillsDir: project Gemini uses .gemini", testProjectGemini),
    example("getSkillsDir: project Windsurf uses .windsurf", testProjectWindsurf),
    example("getSkillsDir: global Windsurf uses .codeium/windsurf (asymmetric)", testGlobalWindsurf),
    example("displaySkillsDir: project Universal", testDisplayProjectUniversal),
    example("displaySkillsDir: global Universal", testDisplayGlobalUniversal),
    example("displaySkillsDir: project Claude", testDisplayProjectClaude),
    example("displaySkillsDir: global Claude", testDisplayGlobalClaude),
    example("displaySkillsDir: project Windsurf", testDisplayProjectWindsurf),
    example("displaySkillsDir: global Windsurf (asymmetric)", testDisplayGlobalWindsurf),
    example("displaySkillsDir: project Copilot", testDisplayProjectCopilot),
    example("displaySkillsDir: global Copilot (asymmetric)", testDisplayGlobalCopilot),
    example("getSearchDirs: returns 14 dirs", testSearchDirsCount),
    example("getSearchDirs: correct priority order", testSearchDirsOrder),
    example("getSearchDirs: first is project universal", testSearchDirsFirst),
  )

  private def testProjectUniversal: Result =
    Dirs.getSkillsDir(Agent.Universal, SkillLocation.Project) ==== (os.pwd / ".agents" / "skills")

  private def testGlobalUniversal: Result =
    Dirs.getSkillsDir(Agent.Universal, SkillLocation.Global) ==== (os.home / ".agents" / "skills")

  private def testProjectClaude: Result =
    Dirs.getSkillsDir(Agent.Claude, SkillLocation.Project) ==== (os.pwd / ".claude" / "skills")

  private def testGlobalClaude: Result =
    Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global) ==== (os.home / ".claude" / "skills")

  private def testProjectCopilot: Result =
    Dirs.getSkillsDir(Agent.Copilot, SkillLocation.Project) ==== (os.pwd / ".github" / "skills")

  private def testGlobalCopilot: Result =
    Dirs.getSkillsDir(Agent.Copilot, SkillLocation.Global) ==== (os.home / ".copilot" / "skills")

  private def testProjectCursor: Result =
    Dirs.getSkillsDir(Agent.Cursor, SkillLocation.Project) ==== (os.pwd / ".cursor" / "skills")

  private def testProjectCodex: Result =
    Dirs.getSkillsDir(Agent.Codex, SkillLocation.Project) ==== (os.pwd / ".codex" / "skills")

  private def testProjectGemini: Result =
    Dirs.getSkillsDir(Agent.Gemini, SkillLocation.Project) ==== (os.pwd / ".gemini" / "skills")

  private def testProjectWindsurf: Result =
    Dirs.getSkillsDir(Agent.Windsurf, SkillLocation.Project) ==== (os.pwd / ".windsurf" / "skills")

  private def testGlobalWindsurf: Result =
    Dirs.getSkillsDir(Agent.Windsurf, SkillLocation.Global) ==== (os.home / ".codeium" / "windsurf" / "skills")

  private def testDisplayProjectUniversal: Result =
    Dirs.displaySkillsDir(Agent.Universal, SkillLocation.Project) ==== ".agents/skills"

  private def testDisplayGlobalUniversal: Result =
    Dirs.displaySkillsDir(Agent.Universal, SkillLocation.Global) ==== "~/.agents/skills"

  private def testDisplayProjectClaude: Result =
    Dirs.displaySkillsDir(Agent.Claude, SkillLocation.Project) ==== ".claude/skills"

  private def testDisplayGlobalClaude: Result =
    Dirs.displaySkillsDir(Agent.Claude, SkillLocation.Global) ==== "~/.claude/skills"

  private def testDisplayProjectWindsurf: Result =
    Dirs.displaySkillsDir(Agent.Windsurf, SkillLocation.Project) ==== ".windsurf/skills"

  private def testDisplayGlobalWindsurf: Result =
    Dirs.displaySkillsDir(Agent.Windsurf, SkillLocation.Global) ==== "~/.codeium/windsurf/skills"

  private def testDisplayProjectCopilot: Result =
    Dirs.displaySkillsDir(Agent.Copilot, SkillLocation.Project) ==== ".github/skills"

  private def testDisplayGlobalCopilot: Result =
    Dirs.displaySkillsDir(Agent.Copilot, SkillLocation.Global) ==== "~/.copilot/skills"

  private def testSearchDirsCount: Result = {
    val dirs = Dirs.getSearchDirs()
    dirs.length ==== 14
  }

  private def testSearchDirsOrder: Result = {
    val dirs = Dirs.getSearchDirs()
    // 1. Project universal
    // 2-7. Project agent-specific (alphabetical: Claude, Codex, Copilot, Cursor, Gemini, Windsurf)
    // 8. Global universal
    // 9-14. Global agent-specific (alphabetical: Claude, Codex, Copilot, Cursor, Gemini, Windsurf)
    Result.all(
      List(
        // Project universal
        dirs(0) ==== (os.pwd / ".agents" / "skills", Agent.Universal, SkillLocation.Project),
        // Project agent-specific (alphabetical)
        dirs(1)._2 ==== Agent.Claude,
        dirs(2)._2 ==== Agent.Codex,
        dirs(3)._2 ==== Agent.Copilot,
        dirs(4)._2 ==== Agent.Cursor,
        dirs(5)._2 ==== Agent.Gemini,
        dirs(6)._2 ==== Agent.Windsurf,
        // All project dirs are Project location
        Result.assert(dirs.take(7).forall(_._3 === SkillLocation.Project)),
        // Global universal
        dirs(7) ==== (os.home / ".agents" / "skills", Agent.Universal, SkillLocation.Global),
        // Global agent-specific (alphabetical)
        dirs(8)._2 ==== Agent.Claude,
        dirs(9)._2 ==== Agent.Codex,
        dirs(10)._2 ==== Agent.Copilot,
        dirs(11)._2 ==== Agent.Cursor,
        dirs(12)._2 ==== Agent.Gemini,
        dirs(13)._2 ==== Agent.Windsurf,
        // All global dirs are Global location
        Result.assert(dirs.drop(7).forall(_._3 === SkillLocation.Global)),
      )
    )
  }

  private def testSearchDirsFirst: Result = {
    val dirs                    = Dirs.getSearchDirs()
    val (path, agent, location) = dirs.head
    Result.all(
      List(
        path ==== (os.pwd / ".agents" / "skills"),
        agent ==== Agent.Universal,
        location ==== SkillLocation.Project,
      )
    )
  }

}
