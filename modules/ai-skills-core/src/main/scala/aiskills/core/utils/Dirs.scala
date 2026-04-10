package aiskills.core.utils

import aiskills.core.{Agent, SkillLocation}

object Dirs {

  /** Get skills directory path for a specific agent. */
  def getSkillsDir(agent: Agent, location: SkillLocation): os.Path =
    location match {
      case SkillLocation.Global => os.home / os.RelPath(agent.globalDirName) / "skills"
      case SkillLocation.Project => os.pwd / os.RelPath(agent.projectDirName) / "skills"
    }

  /** Display-friendly skills directory path for a given agent and location.
    * Project example: ".agents/skills"
    * Global example: "~/.agents/skills"
    */
  def displaySkillsDir(agent: Agent, location: SkillLocation): String =
    location match {
      case SkillLocation.Project => s"${agent.projectDirName}/skills"
      case SkillLocation.Global => s"~/${agent.globalDirName}/skills"
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
    */
  def getSearchDirs(): List[(os.Path, Agent, SkillLocation)] = {
    val agentsSorted = Agent.allNonUniversal.sortBy(_.toString)

    val projectUniversal = List(
      (os.pwd / os.RelPath(Agent.Universal.projectDirName) / "skills", Agent.Universal, SkillLocation.Project)
    )
    val projectSpecific  =
      agentsSorted.map(a => (os.pwd / os.RelPath(a.projectDirName) / "skills", a, SkillLocation.Project))
    val globalUniversal  = List(
      (os.home / os.RelPath(Agent.Universal.globalDirName) / "skills", Agent.Universal, SkillLocation.Global)
    )
    val globalSpecific   =
      agentsSorted.map(a => (os.home / os.RelPath(a.globalDirName) / "skills", a, SkillLocation.Global))

    projectUniversal ++ projectSpecific ++ globalUniversal ++ globalSpecific
  }

}
