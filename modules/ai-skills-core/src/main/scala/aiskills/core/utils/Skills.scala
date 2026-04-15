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
    val dirs = Dirs.getSearchDirs()

    val skills =
      for {
        (dir, agent, location) <- dirs
        if os.exists(dir)
        entry                  <- os.list(dir).toList
        if isDirectoryOrSymlinkToDirectory(entry)
        name      = entry.last
        skillPath = entry / "SKILL.md"
        if os.exists(skillPath)
      } yield {
        val content = os.read(skillPath)
        Skill(
          name = name,
          description = Yaml.extractYamlField(content, "description"),
          location = location,
          agent = agent,
          path = entry,
        )
      }

    skills
  }

  /** Find a specific skill by name. Returns the first match in priority order. */
  def findSkill(skillName: String): Option[SkillLocationInfo] = {
    val dirs = Dirs.getSearchDirs()
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
