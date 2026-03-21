package aiskills.core.utils

import aiskills.core.{Agent, SkillLocation}
import hedgehog.*
import hedgehog.runner.*

object DirsSpec extends Properties:

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
    example("getSearchDirs: returns 12 dirs", testSearchDirsCount),
    example("getSearchDirs: correct priority order", testSearchDirsOrder),
    example("getSearchDirs: first is project universal", testSearchDirsFirst),
    example("getSearchDirs: prefer reorders correctly", testSearchDirsPrefer),
    example("getSearchDirs: prefer None returns default", testSearchDirsPreferNone),
  )

  private def testProjectUniversal: Result =
    Dirs.getSkillsDir(Agent.Universal) ==== (os.pwd / ".agents" / "skills")

  private def testGlobalUniversal: Result =
    Dirs.getSkillsDir(Agent.Universal, global = true) ==== (os.home / ".agents" / "skills")

  private def testProjectClaude: Result =
    Dirs.getSkillsDir(Agent.Claude) ==== (os.pwd / ".claude" / "skills")

  private def testGlobalClaude: Result =
    Dirs.getSkillsDir(Agent.Claude, global = true) ==== (os.home / ".claude" / "skills")

  private def testProjectCopilot: Result =
    Dirs.getSkillsDir(Agent.Copilot) ==== (os.pwd / ".github" / "skills")

  private def testGlobalCopilot: Result =
    Dirs.getSkillsDir(Agent.Copilot, global = true) ==== (os.home / ".copilot" / "skills")

  private def testProjectCursor: Result =
    Dirs.getSkillsDir(Agent.Cursor) ==== (os.pwd / ".cursor" / "skills")

  private def testProjectCodex: Result =
    Dirs.getSkillsDir(Agent.Codex) ==== (os.pwd / ".codex" / "skills")

  private def testProjectGemini: Result =
    Dirs.getSkillsDir(Agent.Gemini) ==== (os.pwd / ".gemini" / "skills")

  private def testSearchDirsCount: Result =
    val dirs = Dirs.getSearchDirs()
    dirs.length ==== 12

  private def testSearchDirsOrder: Result =
    val dirs = Dirs.getSearchDirs()
    // 1. Project universal
    // 2-6. Project agent-specific (alphabetical: Claude, Codex, Copilot, Cursor, Gemini)
    // 7. Global universal
    // 8-12. Global agent-specific (alphabetical: Claude, Codex, Copilot, Cursor, Gemini)
    Result.all(List(
      // Project universal
      dirs(0) ==== ((os.pwd / ".agents" / "skills", Agent.Universal, SkillLocation.Project)),
      // Project agent-specific (alphabetical)
      dirs(1)._2 ==== Agent.Claude,
      dirs(2)._2 ==== Agent.Codex,
      dirs(3)._2 ==== Agent.Copilot,
      dirs(4)._2 ==== Agent.Cursor,
      dirs(5)._2 ==== Agent.Gemini,
      // All project dirs are Project location
      Result.assert(dirs.take(6).forall(_._3 == SkillLocation.Project)),
      // Global universal
      dirs(6) ==== ((os.home / ".agents" / "skills", Agent.Universal, SkillLocation.Global)),
      // Global agent-specific (alphabetical)
      dirs(7)._2 ==== Agent.Claude,
      dirs(8)._2 ==== Agent.Codex,
      dirs(9)._2 ==== Agent.Copilot,
      dirs(10)._2 ==== Agent.Cursor,
      dirs(11)._2 ==== Agent.Gemini,
      // All global dirs are Global location
      Result.assert(dirs.drop(6).forall(_._3 == SkillLocation.Global)),
    ))

  private def testSearchDirsFirst: Result =
    val dirs = Dirs.getSearchDirs()
    val (path, agent, location) = dirs.head
    Result.all(List(
      path ==== (os.pwd / ".agents" / "skills"),
      agent ==== Agent.Universal,
      location ==== SkillLocation.Project,
    ))

  private def testSearchDirsPrefer: Result =
    val dirs = Dirs.getSearchDirs(Some(Agent.Cursor))
    // Cursor dirs should be first
    val (firstPath, firstAgent, _) = dirs.head
    Result.all(List(
      firstAgent ==== Agent.Cursor,
      // Should still have 12 dirs
      dirs.length ==== 12,
    ))

  private def testSearchDirsPreferNone: Result =
    val defaultDirs = Dirs.getSearchDirs()
    val preferNoneDirs = Dirs.getSearchDirs(None)
    defaultDirs ==== preferNoneDirs
