package aiskills.core.utils

import aiskills.core.{Agent, GlobalDirOverride, GlobalDirResolution, SkillLocation, given}
import cats.syntax.all.*

object Dirs {

  /** Parse an env var path value (already trimmed and non-empty).
    * "~" and "~/..." expand to home; relative values resolve against pwd.
    */
  private def parseEnvPath(value: String): os.Path =
    if value === "~" then os.home
    else if value.startsWith("~/") then os.home / os.RelPath(value.drop(2))
    else os.Path(value, os.pwd)

  /** Resolve the global config base dir for an agent, honoring its own env var
    * (e.g. CLAUDE_CONFIG_DIR) when set and non-blank.
    */
  def resolveGlobalBase(
    agent: Agent,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): GlobalDirResolution = {
    val default = GlobalDirResolution(os.home / os.RelPath(agent.globalDirName), none)
    agent.globalDirOverride match {
      case GlobalDirOverride.ConfigDir(name) =>
        env
          .get(name)
          .map(_.trim)
          .filter(_.nonEmpty)
          .fold(default)(value => GlobalDirResolution(parseEnvPath(value), name.some))
      case GlobalDirOverride.HomeRoot(name) =>
        env
          .get(name)
          .map(_.trim)
          .filter(_.nonEmpty)
          .fold(default)(value => GlobalDirResolution(parseEnvPath(value) / os.RelPath(agent.globalDirName), name.some))
      case GlobalDirOverride.NoOverride =>
        default
    }
  }

  /** Env var name when the global dir for this agent is currently overridden. */
  def globalEnvVar(
    agent: Agent,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): Option[String] =
    resolveGlobalBase(agent, env).envVar

  /** Get skills directory path for a specific agent. */
  def getSkillsDir(
    agent: Agent,
    location: SkillLocation,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): os.Path =
    location match {
      case SkillLocation.Global => resolveGlobalBase(agent, env).configBase / "skills"
      case SkillLocation.Project => os.pwd / os.RelPath(agent.projectDirName) / "skills"
    }

  /** Display-friendly global config base dir for an agent.
    * Default example: "~/.claude"
    * Overridden examples: "$CLAUDE_CONFIG_DIR", "$GEMINI_CLI_HOME/.gemini"
    */
  def displayGlobalBase(
    agent: Agent,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): String =
    resolveGlobalBase(agent, env).envVar match {
      case Some(name) =>
        agent.globalDirOverride match {
          case GlobalDirOverride.HomeRoot(_) => s"$$$name/${agent.globalDirName}"
          case GlobalDirOverride.ConfigDir(_) | GlobalDirOverride.NoOverride => s"$$$name"
        }
      case None => s"~/${agent.globalDirName}"
    }

  /** Like displayGlobalBase, but with the resolved dir appended when overridden.
    * Examples: "$CLAUDE_CONFIG_DIR=~/claude-work", "$GEMINI_CLI_HOME/.gemini=~/groot/.gemini", "~/.codex"
    */
  def displayGlobalBaseResolved(
    agent: Agent,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): String = {
    val resolution = resolveGlobalBase(agent, env)
    resolution.envVar match {
      case Some(_) => s"${displayGlobalBase(agent, env)}=${displayPath(resolution.configBase)}"
      case None => displayGlobalBase(agent, env)
    }
  }

  /** Display-friendly skills directory path for a given agent and location.
    * Project example: ".agents/skills"
    * Global examples: "~/.agents/skills", "$CLAUDE_CONFIG_DIR/skills" (when overridden)
    */
  def displaySkillsDir(
    agent: Agent,
    location: SkillLocation,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): String =
    location match {
      case SkillLocation.Project => s"${agent.projectDirName}/skills"
      case SkillLocation.Global => s"${displayGlobalBase(agent, env)}/skills"
    }

  /** Env var name to annotate a path with, when the path lives under an
    * agent's overridden global config dir.
    */
  def envVarAnnotationFor(
    path: os.Path,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): Option[String] =
    Agent
      .all
      .collectFirstSome { agent =>
        val resolution = resolveGlobalBase(agent, env)
        resolution.envVar.filter(_ => path.startsWith(resolution.configBase))
      }

  /** Display-friendly path: replaces home prefix with ~, or shows relative to pwd if possible. */
  def displayPath(path: os.Path): String =
    if path.startsWith(os.home) then "~" + path.toString.stripPrefix(os.home.toString)
    else if path.startsWith(os.pwd) then path.relativeTo(os.pwd).toString
    else path.toString

  /** Get all searchable skill directories in priority order.
    * Priority:
    *   1. Project universal (.agents)
    *   2. Project agent-specific (alphabetical by agent name)
    *   3. Global universal (~/.agents)
    *   4. Global agent-specific (alphabetical by agent name)
    *
    * When pwd is the home directory, project entries are omitted
    * because they would resolve to the same paths as global entries.
    */
  def getSearchDirs(): List[(os.Path, Agent, SkillLocation)] =
    getSearchDirs(os.pwd)

  def getSearchDirs(
    pwd: os.Path,
    env: Map[String, String] = sys.env // scalafix:ok DisableSyntax.defaultArgs
  ): List[(os.Path, Agent, SkillLocation)] = {
    val agentsSorted = Agent.allNonUniversal.sortBy(_.toString)

    val globalUniversal = List(
      (getSkillsDir(Agent.Universal, SkillLocation.Global, env), Agent.Universal, SkillLocation.Global)
    )
    val globalSpecific  =
      agentsSorted.map(a => (getSkillsDir(a, SkillLocation.Global, env), a, SkillLocation.Global))

    if pwd === os.home then (globalUniversal ++ globalSpecific).distinctBy { case (path, _, _) => path }
    else {
      val projectUniversal = List(
        (pwd / os.RelPath(Agent.Universal.projectDirName) / "skills", Agent.Universal, SkillLocation.Project)
      )
      val projectSpecific  =
        agentsSorted.map(a => (pwd / os.RelPath(a.projectDirName) / "skills", a, SkillLocation.Project))

      (projectUniversal ++ projectSpecific ++ globalUniversal ++ globalSpecific).distinctBy {
        case (path, _, _) => path
      }
    }
  }

}
