package aiskills.core.utils

import aiskills.core.{Agent, Skill, SkillLocation, SkillLocationInfo}
import cats.syntax.all.*

import scala.util.Try

object Skills {

  /** Check if a path is a directory (follows symlinks). Returns false for broken symlinks. */
  private def isDirectoryOrSymlinkToDirectory(path: os.Path): Boolean =
    Try(os.isDir(path)).getOrElse(false)

  /** Find all installed skills across all agent directories. No deduplication. */
  def findAllSkills(): List[Skill] = {
    val dirs   = Dirs.getSearchDirs()
    val skills = List.newBuilder[Skill]

    for {
      (dir, agent, location) <- dirs
      if os.exists(dir)
      entry                  <- os.list(dir)
      if isDirectoryOrSymlinkToDirectory(entry)
      name      = entry.last
      skillPath = entry / "SKILL.md"
      if os.exists(skillPath)
    } do {
      val content = os.read(skillPath)
      skills += Skill(
        name = name,
        description = Yaml.extractYamlField(content, "description"),
        location = location,
        agent = agent,
        path = entry,
      )
    }

    skills.result()
  }

  /** Find a specific skill by name. Returns the first match in priority order. */
  def findSkill(skillName: String): Option[SkillLocationInfo] =
    findSkill(skillName, prefer = none[Agent])

  /** Find a specific skill by name with optional agent preference. */
  def findSkill(skillName: String, prefer: Option[Agent]): Option[SkillLocationInfo] = {
    val dirs = Dirs.getSearchDirs(prefer)
    dirs
      .iterator
      .map { case (dir, agent, location) => (dir / skillName / "SKILL.md", agent, location) }
      .find { case (skillPath, _, _) => os.exists(skillPath) }
      .map {
        case (skillPath, agent, location) =>
          SkillLocationInfo(
            path = skillPath,
            baseDir = skillPath / os.up,
            source = skillPath / os.up / os.up,
            agent = agent,
            location = location,
          )
      }
  }

  /** Find all locations where a skill with the given name exists. */
  def findAllSkillLocations(skillName: String): List[(os.Path, Agent, SkillLocation)] =
    Dirs.getSearchDirs().filter {
      case (dir, _, _) =>
        os.exists(dir / skillName / "SKILL.md")
    }

  /** Find all skills installed for a specific agent. */
  def findSkillsByAgent(agent: Agent, location: SkillLocation): List[Skill] = {
    val dir = Dirs.getSkillsDir(agent, location)
    if !os.exists(dir) then Nil
    else
      os.list(dir).toList.flatMap { entry =>
        if !isDirectoryOrSymlinkToDirectory(entry) then none[Skill]
        else {
          val skillPath = entry / "SKILL.md"
          if !os.exists(skillPath) then none[Skill]
          else {
            val content = os.read(skillPath)
            Skill(
              name = entry.last,
              description = Yaml.extractYamlField(content, "description"),
              location = location,
              agent = agent,
              path = entry,
            ).some
          }
        }
      }
  }
}
