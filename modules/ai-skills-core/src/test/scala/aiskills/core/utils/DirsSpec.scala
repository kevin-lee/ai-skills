package aiskills.core.utils

import aiskills.core.{Agent, DedupedTargets, SkillLocation, SkippedTarget, given}
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
    example("getSkillsDir: project Antigravity uses .agents (same as Universal)", testProjectAntigravity),
    example("getSkillsDir: project Gemini uses .gemini", testProjectGemini),
    example("getSkillsDir: project Windsurf uses .windsurf", testProjectWindsurf),
    example("getSkillsDir: global Antigravity uses .gemini/config (asymmetric)", testGlobalAntigravity),
    example("getSkillsDir: global Windsurf uses .codeium/windsurf (asymmetric)", testGlobalWindsurf),
    example("displaySkillsDir: project Universal", testDisplayProjectUniversal),
    example("displaySkillsDir: global Universal", testDisplayGlobalUniversal),
    example("displaySkillsDir: project Claude", testDisplayProjectClaude),
    example("displaySkillsDir: global Claude", testDisplayGlobalClaude),
    example("displaySkillsDir: project Antigravity", testDisplayProjectAntigravity),
    example("displaySkillsDir: global Antigravity (asymmetric)", testDisplayGlobalAntigravity),
    example("displaySkillsDir: project Windsurf", testDisplayProjectWindsurf),
    example("displaySkillsDir: global Windsurf (asymmetric)", testDisplayGlobalWindsurf),
    example("displaySkillsDir: project Copilot", testDisplayProjectCopilot),
    example("displaySkillsDir: global Copilot (asymmetric)", testDisplayGlobalCopilot),
    example("displayPath: global path replaces home with ~", testDisplayPathGlobal),
    example("displayPath: project path shows relative to pwd", testDisplayPathProject),
    example("displayPath: only replaces home prefix, not duplicates in path", testDisplayPathHomeDuplicate),
    example("getSearchDirs: returns 15 dirs", testSearchDirsCount),
    example("getSearchDirs: correct priority order", testSearchDirsOrder),
    example(
      "getSearchDirs: project Antigravity is merged into project Universal",
      testSearchDirsProjectAntigravityMerged
    ),
    example("getSearchDirs: first is project universal", testSearchDirsFirst),
    example("getSearchDirs: returns 8 global-only dirs when pwd is home", testSearchDirsPwdIsHome),
    example("getSearchDirs: returns 15 dirs when pwd is not home", testSearchDirsPwdIsNotHome),
    example("getSearchDirs: no-arg delegates to overload with os.pwd", testSearchDirsNoArgDelegates),
    example("getSkillsDir: global Claude honors CLAUDE_CONFIG_DIR", testEnvClaudeConfigDir),
    example("getSkillsDir: global Codex honors CODEX_HOME", testEnvCodexHome),
    example("getSkillsDir: global Copilot honors COPILOT_HOME", testEnvCopilotHome),
    example("getSkillsDir: global Gemini honors GEMINI_CLI_HOME as home root", testEnvGeminiCliHome),
    example("getSkillsDir: global Cursor does not honor CURSOR_CONFIG_DIR", testEnvCursorNotHonored),
    example(
      "getSkillsDir: global Universal, Antigravity and Windsurf ignore all honored env vars",
      testEnvUniversalAntigravityWindsurfNotHonored
    ),
    example("getSkillsDir: blank or empty env var value falls back to default", testEnvBlankValue),
    example("getSkillsDir: env var value with ~ expands to home", testEnvTildeExpansion),
    example("getSkillsDir: relative env var value resolves against pwd", testEnvRelativeValue),
    example("getSkillsDir: project location ignores env vars", testEnvProjectUnaffected),
    example("displaySkillsDir: global shows env var form when overridden", testDisplaySkillsDirOverridden),
    example("displayGlobalBase: env var form when overridden, ~ form otherwise", testDisplayGlobalBase),
    example("displayGlobalBaseResolved: shows VAR=resolved dir when overridden", testDisplayGlobalBaseResolved),
    example("globalEnvVar: Some when overridden, None otherwise", testGlobalEnvVar),
    example("envVarAnnotationFor: Some for paths under overridden base", testEnvVarAnnotationFor),
    example("getSearchDirs: global entries honor env overrides", testSearchDirsEnvOverride),
    example("getSearchDirs: dedup when override collides with project dir", testSearchDirsEnvOverrideDedup),
    example("sharedSkillsDirAgents: Universal and Antigravity share the project dir only", testSharedSkillsDirAgents),
    example("dedupeTargetsBySkillsDir: keeps the first target for each dir", testDedupeTargetsBySkillsDir),
    example(
      "dedupeTargetsBySkillsDir: env override that makes global Claude the project dir",
      testDedupeTargetsEnvOverrideCollision
    ),
    example(
      "dedupeAgentLocationTargets: agent first, then location, with the shared dir skipped",
      testDedupeAgentLocationTargets
    ),
  )

  private def testProjectUniversal: Result =
    Dirs.getSkillsDir(Agent.Universal, SkillLocation.Project) ==== (os.pwd / ".agents" / "skills")

  private val emptyEnv = Map.empty[String, String]

  private def testGlobalUniversal: Result =
    Dirs.getSkillsDir(Agent.Universal, SkillLocation.Global, emptyEnv) ==== (os.home / ".agents" / "skills")

  private def testProjectClaude: Result =
    Dirs.getSkillsDir(Agent.Claude, SkillLocation.Project) ==== (os.pwd / ".claude" / "skills")

  private def testGlobalClaude: Result =
    Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global, emptyEnv) ==== (os.home / ".claude" / "skills")

  private def testProjectCopilot: Result =
    Dirs.getSkillsDir(Agent.Copilot, SkillLocation.Project) ==== (os.pwd / ".github" / "skills")

  private def testGlobalCopilot: Result =
    Dirs.getSkillsDir(Agent.Copilot, SkillLocation.Global, emptyEnv) ==== (os.home / ".copilot" / "skills")

  private def testProjectCursor: Result =
    Dirs.getSkillsDir(Agent.Cursor, SkillLocation.Project) ==== (os.pwd / ".cursor" / "skills")

  private def testProjectCodex: Result =
    Dirs.getSkillsDir(Agent.Codex, SkillLocation.Project) ==== (os.pwd / ".codex" / "skills")

  private def testProjectAntigravity: Result =
    Dirs.getSkillsDir(Agent.Antigravity, SkillLocation.Project) ==== (os.pwd / ".agents" / "skills")

  private def testGlobalAntigravity: Result =
    Dirs.getSkillsDir(
      Agent.Antigravity,
      SkillLocation.Global,
      emptyEnv
    ) ==== (os.home / ".gemini" / "config" / "skills")

  private def testProjectGemini: Result =
    Dirs.getSkillsDir(Agent.Gemini, SkillLocation.Project) ==== (os.pwd / ".gemini" / "skills")

  private def testProjectWindsurf: Result =
    Dirs.getSkillsDir(Agent.Windsurf, SkillLocation.Project) ==== (os.pwd / ".windsurf" / "skills")

  private def testGlobalWindsurf: Result =
    Dirs.getSkillsDir(
      Agent.Windsurf,
      SkillLocation.Global,
      emptyEnv
    ) ==== (os.home / ".codeium" / "windsurf" / "skills")

  private def testDisplayProjectUniversal: Result =
    Dirs.displaySkillsDir(Agent.Universal, SkillLocation.Project) ==== ".agents/skills"

  private def testDisplayGlobalUniversal: Result =
    Dirs.displaySkillsDir(Agent.Universal, SkillLocation.Global, emptyEnv) ==== "~/.agents/skills"

  private def testDisplayProjectClaude: Result =
    Dirs.displaySkillsDir(Agent.Claude, SkillLocation.Project) ==== ".claude/skills"

  private def testDisplayGlobalClaude: Result =
    Dirs.displaySkillsDir(Agent.Claude, SkillLocation.Global, emptyEnv) ==== "~/.claude/skills"

  private def testDisplayProjectAntigravity: Result =
    Dirs.displaySkillsDir(Agent.Antigravity, SkillLocation.Project) ==== ".agents/skills"

  private def testDisplayGlobalAntigravity: Result =
    Dirs.displaySkillsDir(Agent.Antigravity, SkillLocation.Global, emptyEnv) ==== "~/.gemini/config/skills"

  private def testDisplayProjectWindsurf: Result =
    Dirs.displaySkillsDir(Agent.Windsurf, SkillLocation.Project) ==== ".windsurf/skills"

  private def testDisplayGlobalWindsurf: Result =
    Dirs.displaySkillsDir(Agent.Windsurf, SkillLocation.Global, emptyEnv) ==== "~/.codeium/windsurf/skills"

  private def testDisplayProjectCopilot: Result =
    Dirs.displaySkillsDir(Agent.Copilot, SkillLocation.Project) ==== ".github/skills"

  private def testDisplayGlobalCopilot: Result =
    Dirs.displaySkillsDir(Agent.Copilot, SkillLocation.Global, emptyEnv) ==== "~/.copilot/skills"

  private def testDisplayPathGlobal: Result =
    Dirs.displayPath(os.home / ".claude" / "skills" / "foo") ==== "~/.claude/skills/foo"

  private def testDisplayPathProject: Result =
    Dirs.displayPath(os.root / "tmp" / ".claude" / "skills" / "foo") ==== "/tmp/.claude/skills/foo"

  private def testDisplayPathHomeDuplicate: Result = {
    val homeStr  = os.home.toString
    // e.g. /Users/username/blah/Users/username/something/.claude/skills/foo
    val path     = os.Path(s"$homeStr/blah$homeStr/something/.claude/skills/foo")
    val expected = s"~/blah$homeStr/something/.claude/skills/foo"
    Dirs.displayPath(path) ==== expected
  }

  private val nonHomePwd = os.root / "some" / "project"

  private def testSearchDirsCount: Result = {
    val dirs = Dirs.getSearchDirs(nonHomePwd, emptyEnv)
    dirs.length ==== 15
  }

  private def testSearchDirsOrder: Result = {
    val dirs   = Dirs.getSearchDirs(nonHomePwd, emptyEnv)
    val agents = dirs.map { case (_, agent, _) => agent }
    // 1. Project universal (project Antigravity uses the same .agents/skills, so it is merged into this entry)
    // 2-7. Project agent-specific (alphabetical, except Antigravity directly before Gemini:
    //      Claude, Codex, Copilot, Cursor, Gemini, Windsurf)
    // 8. Global universal
    // 9-15. Global agent-specific (alphabetical, except Antigravity directly before Gemini:
    //       Claude, Codex, Copilot, Cursor, Antigravity, Gemini, Windsurf)
    Result.all(
      List(
        // Project universal
        dirs.headOption ==== (nonHomePwd / ".agents" / "skills", Agent.Universal, SkillLocation.Project).some,
        // Project agent-specific
        agents(1) ==== Agent.Claude,
        agents(2) ==== Agent.Codex,
        agents(3) ==== Agent.Copilot,
        agents(4) ==== Agent.Cursor,
        agents(5) ==== Agent.Gemini,
        agents(6) ==== Agent.Windsurf,
        // All project dirs are Project location
        Result.assert(dirs.take(7).forall { case (_, _, location) => location === SkillLocation.Project }),
        // Global universal
        dirs(7) ==== (os.home / ".agents" / "skills", Agent.Universal, SkillLocation.Global),
        // Global agent-specific
        agents(8) ==== Agent.Claude,
        agents(9) ==== Agent.Codex,
        agents(10) ==== Agent.Copilot,
        agents(11) ==== Agent.Cursor,
        agents(12) ==== Agent.Antigravity,
        agents(13) ==== Agent.Gemini,
        agents(14) ==== Agent.Windsurf,
        // All global dirs are Global location
        Result.assert(dirs.drop(7).forall { case (_, _, location) => location === SkillLocation.Global }),
      )
    )
  }

  private def testSearchDirsProjectAntigravityMerged: Result = {
    val dirs              = Dirs.getSearchDirs(nonHomePwd, emptyEnv)
    val projectAgentsDirs = dirs.filter { case (path, _, _) => path === (nonHomePwd / ".agents" / "skills") }
    Result.all(
      List(
        Result.assert(!dirs.exists {
          case (_, agent, location) => agent === Agent.Antigravity && location === SkillLocation.Project
        }),
        projectAgentsDirs ==== List(
          (nonHomePwd / ".agents" / "skills", Agent.Universal, SkillLocation.Project)
        ),
      )
    )
  }

  private def testSearchDirsFirst: Result = {
    val dirs                    = Dirs.getSearchDirs(nonHomePwd, emptyEnv)
    val (path, agent, location) = dirs.head
    Result.all(
      List(
        path ==== (nonHomePwd / ".agents" / "skills"),
        agent ==== Agent.Universal,
        location ==== SkillLocation.Project,
      )
    )
  }

  private def testSearchDirsPwdIsHome: Result = {
    val dirs = Dirs.getSearchDirs(os.home, emptyEnv)
    Result.all(
      List(
        dirs.length ==== 8,
        Result.assert(dirs.forall { case (_, _, location) => location === SkillLocation.Global }),
      )
    )
  }

  private def testSearchDirsPwdIsNotHome: Result = {
    val dirs = Dirs.getSearchDirs(nonHomePwd, emptyEnv)
    Result.all(
      List(
        dirs.length ==== 15,
        Result.assert(dirs.take(7).forall { case (_, _, location) => location === SkillLocation.Project }),
        Result.assert(dirs.drop(7).forall { case (_, _, location) => location === SkillLocation.Global }),
      )
    )
  }

  private def testSearchDirsNoArgDelegates: Result = {
    val noArg   = Dirs.getSearchDirs()
    val withPwd = Dirs.getSearchDirs(os.pwd)
    noArg ==== withPwd
  }

  private def testEnvClaudeConfigDir: Result = {
    val env = Map("CLAUDE_CONFIG_DIR" -> "/custom/claude")
    Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global, env) ==== (os.root / "custom" / "claude" / "skills")
  }

  private def testEnvCodexHome: Result = {
    val env = Map("CODEX_HOME" -> "/custom/codex")
    Dirs.getSkillsDir(Agent.Codex, SkillLocation.Global, env) ==== (os.root / "custom" / "codex" / "skills")
  }

  private def testEnvCopilotHome: Result = {
    val env = Map("COPILOT_HOME" -> "/custom/copilot")
    Dirs.getSkillsDir(Agent.Copilot, SkillLocation.Global, env) ==== (os.root / "custom" / "copilot" / "skills")
  }

  private def testEnvGeminiCliHome: Result = {
    val env = Map("GEMINI_CLI_HOME" -> "/custom/root")
    Dirs.getSkillsDir(Agent.Gemini, SkillLocation.Global, env) ====
      (os.root / "custom" / "root" / ".gemini" / "skills")
  }

  private def testEnvCursorNotHonored: Result = {
    val env = Map("CURSOR_CONFIG_DIR" -> "/custom/cursor")
    Dirs.getSkillsDir(Agent.Cursor, SkillLocation.Global, env) ==== (os.home / ".cursor" / "skills")
  }

  private val allHonoredEnvVars = Map(
    "CLAUDE_CONFIG_DIR" -> "/custom/claude",
    "CODEX_HOME"        -> "/custom/codex",
    "GEMINI_CLI_HOME"   -> "/custom/root",
    "COPILOT_HOME"      -> "/custom/copilot",
  )

  private def testEnvUniversalAntigravityWindsurfNotHonored: Result =
    Result.all(
      List(
        Dirs.getSkillsDir(Agent.Universal, SkillLocation.Global, allHonoredEnvVars) ====
          (os.home / ".agents" / "skills"),
        Dirs.getSkillsDir(Agent.Antigravity, SkillLocation.Global, allHonoredEnvVars) ====
          (os.home / ".gemini" / "config" / "skills"),
        Dirs.getSkillsDir(Agent.Windsurf, SkillLocation.Global, allHonoredEnvVars) ====
          (os.home / ".codeium" / "windsurf" / "skills"),
      )
    )

  private def testEnvBlankValue: Result =
    Result.all(
      List(
        Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global, Map("CLAUDE_CONFIG_DIR" -> "  ")) ====
          (os.home / ".claude" / "skills"),
        Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global, Map("CLAUDE_CONFIG_DIR" -> "")) ====
          (os.home / ".claude" / "skills"),
      )
    )

  private def testEnvTildeExpansion: Result =
    Result.all(
      List(
        Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global, Map("CLAUDE_CONFIG_DIR" -> "~/custom-claude")) ====
          (os.home / "custom-claude" / "skills"),
        Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global, Map("CLAUDE_CONFIG_DIR" -> "~")) ====
          (os.home / "skills"),
      )
    )

  private def testEnvRelativeValue: Result =
    Dirs.getSkillsDir(Agent.Claude, SkillLocation.Global, Map("CLAUDE_CONFIG_DIR" -> "custom/dir")) ====
      (os.pwd / "custom" / "dir" / "skills")

  private def testEnvProjectUnaffected: Result =
    Result.all(
      List(
        Dirs.getSkillsDir(Agent.Claude, SkillLocation.Project, allHonoredEnvVars) ====
          (os.pwd / ".claude" / "skills"),
        Dirs.getSkillsDir(Agent.Gemini, SkillLocation.Project, allHonoredEnvVars) ====
          (os.pwd / ".gemini" / "skills"),
      )
    )

  private def testDisplaySkillsDirOverridden: Result =
    Result.all(
      List(
        Dirs.displaySkillsDir(Agent.Claude, SkillLocation.Global, allHonoredEnvVars) ====
          "$CLAUDE_CONFIG_DIR/skills",
        Dirs.displaySkillsDir(Agent.Gemini, SkillLocation.Global, allHonoredEnvVars) ====
          "$GEMINI_CLI_HOME/.gemini/skills",
        Dirs.displaySkillsDir(Agent.Claude, SkillLocation.Global, emptyEnv) ==== "~/.claude/skills",
      )
    )

  private def testDisplayGlobalBase: Result =
    Result.all(
      List(
        Dirs.displayGlobalBase(Agent.Claude, allHonoredEnvVars) ==== "$CLAUDE_CONFIG_DIR",
        Dirs.displayGlobalBase(Agent.Gemini, allHonoredEnvVars) ==== "$GEMINI_CLI_HOME/.gemini",
        Dirs.displayGlobalBase(Agent.Claude, emptyEnv) ==== "~/.claude",
      )
    )

  private def testDisplayGlobalBaseResolved: Result =
    Result.all(
      List(
        Dirs.displayGlobalBaseResolved(Agent.Claude, Map("CLAUDE_CONFIG_DIR" -> "/custom/claude")) ====
          "$CLAUDE_CONFIG_DIR=/custom/claude",
        Dirs.displayGlobalBaseResolved(Agent.Claude, Map("CLAUDE_CONFIG_DIR" -> "~/custom-claude")) ====
          "$CLAUDE_CONFIG_DIR=~/custom-claude",
        Dirs.displayGlobalBaseResolved(Agent.Gemini, Map("GEMINI_CLI_HOME" -> "/custom/root")) ====
          "$GEMINI_CLI_HOME/.gemini=/custom/root/.gemini",
        Dirs.displayGlobalBaseResolved(Agent.Claude, emptyEnv) ==== "~/.claude",
      )
    )

  private def testGlobalEnvVar: Result =
    Result.all(
      List(
        Dirs.globalEnvVar(Agent.Claude, allHonoredEnvVars) ==== Some("CLAUDE_CONFIG_DIR"),
        Dirs.globalEnvVar(Agent.Claude, emptyEnv) ==== None,
        Dirs.globalEnvVar(Agent.Cursor, Map("CURSOR_CONFIG_DIR" -> "/custom/cursor")) ==== None,
      )
    )

  private def testEnvVarAnnotationFor: Result = {
    val env = Map("CLAUDE_CONFIG_DIR" -> "/custom/claude")
    Result.all(
      List(
        Dirs.envVarAnnotationFor(os.root / "custom" / "claude" / "skills" / "foo", env) ====
          Some("CLAUDE_CONFIG_DIR"),
        Dirs.envVarAnnotationFor(os.root / "unrelated" / "path", env) ==== None,
        Dirs.envVarAnnotationFor(os.root / "custom" / "claude" / "skills" / "foo", emptyEnv) ==== None,
      )
    )
  }

  private def testSearchDirsEnvOverride: Result = {
    val env  = Map("CLAUDE_CONFIG_DIR" -> "/custom/claude")
    val dirs = Dirs.getSearchDirs(nonHomePwd, env)
    Result.all(
      List(
        dirs.length ==== 15,
        dirs(8) ==== (os.root / "custom" / "claude" / "skills", Agent.Claude, SkillLocation.Global),
        Result.assert(dirs.take(7).forall {
          case (path, _, location) =>
            location === SkillLocation.Project && path.startsWith(nonHomePwd)
        }),
      )
    )
  }

  private def testSearchDirsEnvOverrideDedup: Result = {
    val env                 = Map("CLAUDE_CONFIG_DIR" -> (nonHomePwd / ".claude").toString)
    val dirs                = Dirs.getSearchDirs(nonHomePwd, env)
    val claudeSkillsEntries = dirs.filter { case (path, _, _) => path === (nonHomePwd / ".claude" / "skills") }
    Result.all(
      List(
        dirs.length ==== 14,
        claudeSkillsEntries.map { case (_, agent, location) => (agent, location) } ====
          List((Agent.Claude, SkillLocation.Project)),
      )
    )
  }

  private def testSharedSkillsDirAgents: Result =
    Result.all(
      List(
        Dirs.sharedSkillsDirAgents(Agent.Antigravity, SkillLocation.Project, emptyEnv) ==== List(Agent.Universal),
        Dirs.sharedSkillsDirAgents(Agent.Universal, SkillLocation.Project, emptyEnv) ==== List(Agent.Antigravity),
        Dirs.sharedSkillsDirAgents(Agent.Antigravity, SkillLocation.Global, emptyEnv) ==== Nil,
        Dirs.sharedSkillsDirAgents(Agent.Claude, SkillLocation.Project, emptyEnv) ==== Nil,
      )
    )

  private def testDedupeTargetsBySkillsDir: Result = {
    val targets = List(
      (Agent.Universal, SkillLocation.Project),
      (Agent.Antigravity, SkillLocation.Project),
      (Agent.Antigravity, SkillLocation.Global),
      (Agent.Claude, SkillLocation.Project),
    )
    Dirs.dedupeTargetsBySkillsDir(targets, emptyEnv) ==== DedupedTargets(
      kept = List(
        (Agent.Universal, SkillLocation.Project),
        (Agent.Antigravity, SkillLocation.Global),
        (Agent.Claude, SkillLocation.Project),
      ),
      skipped = List(SkippedTarget(Agent.Antigravity, SkillLocation.Project, Agent.Universal)),
    )
  }

  private def testDedupeTargetsEnvOverrideCollision: Result = {
    val env     = Map("CLAUDE_CONFIG_DIR" -> (os.pwd / ".claude").toString)
    val targets = List(
      (Agent.Claude, SkillLocation.Project),
      (Agent.Claude, SkillLocation.Global),
    )
    Dirs.dedupeTargetsBySkillsDir(targets, env) ==== DedupedTargets(
      kept = List((Agent.Claude, SkillLocation.Project)),
      skipped = List(SkippedTarget(Agent.Claude, SkillLocation.Global, Agent.Claude)),
    )
  }

  private def testDedupeAgentLocationTargets: Result =
    Dirs.dedupeAgentLocationTargets(
      List(Agent.Universal, Agent.Antigravity),
      List(SkillLocation.Project, SkillLocation.Global),
      emptyEnv,
    ) ==== DedupedTargets(
      kept = List(
        (Agent.Universal, SkillLocation.Project),
        (Agent.Universal, SkillLocation.Global),
        (Agent.Antigravity, SkillLocation.Global),
      ),
      skipped = List(SkippedTarget(Agent.Antigravity, SkillLocation.Project, Agent.Universal)),
    )

}
