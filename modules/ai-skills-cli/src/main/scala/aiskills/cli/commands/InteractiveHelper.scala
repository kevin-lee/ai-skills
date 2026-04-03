package aiskills.cli.commands

import aiskills.core.{Agent, Skill, SkillLocation}
import aiskills.core.utils.Dirs
import cats.syntax.all.*
import extras.scala.io.syntax.color.*

object InteractiveHelper {

  /** Pure decision: returns Some(location) if only one location has skills, None if both do. */
  def resolveLocations(allSkills: List[Skill]): Option[SkillLocation] = {
    val hasProject = allSkills.exists(_.location === SkillLocation.Project)
    val hasGlobal  = allSkills.exists(_.location === SkillLocation.Global)
    (hasProject, hasGlobal) match {
      case (true, false) => SkillLocation.Project.some
      case (false, true) => SkillLocation.Global.some
      case _ => none
    }
  }

  /** Pure decision: returns Some(agent) if only one agent has skills, None if multiple do. */
  def resolveAgents(agentsWithCounts: List[(Agent, Int)]): Option[Agent] =
    agentsWithCounts match {
      case (agent, _) :: Nil => agent.some
      case _ => none
    }

  /** Build colored location path labels for an agent (for println output).
    * e.g. List("(project, Claude): .claude/skills", "(global, Claude): ~/.claude/skills")
    */
  def agentLocationLabels(agent: Agent, locations: List[SkillLocation]): List[String] =
    locations.sortBy(_.ordinal).map { loc =>
      s"(${loc.toString.toLowerCase}, ${agent.toString})".blue + s": ${Dirs.displaySkillsDir(agent, loc)}".dim
    }

  /** Build plain-text location path labels for an agent (for prompt labels).
    * e.g. List("(project, Claude): .claude/skills", "(global, Claude): ~/.claude/skills")
    */
  def agentLocationLabelsPlain(agent: Agent, locations: List[SkillLocation]): List[String] =
    locations.sortBy(_.ordinal).map { loc =>
      s"(${loc.toString.toLowerCase}, ${agent.toString}): ${Dirs.displaySkillsDir(agent, loc)}"
    }

  /** Build an agent label for interactive prompts (plain text, no ANSI colors).
    * e.g. "Claude          (4 skill(s))  (project, Claude): .claude/skills, (global, Claude): ~/.claude/skills"
    */
  def buildAgentLabel(agent: Agent, count: Int, skillsInScope: List[Skill]): String = {
    val agentLocations = skillsInScope.filter(_.agent === agent).map(_.location).distinct.sortBy(_.ordinal)
    val pathParts      = agentLocationLabelsPlain(agent, agentLocations)
    s"${agent.toString.padTo(15, ' ')} ($count skill(s))  ${pathParts.mkString(", ")}"
  }

  /** Report location resolution result, then delegate to continuation.
    * If resolved (Some), prints an auto-select message before calling f.
    * If not resolved (None), calls f directly (caller handles prompting).
    */
  def reportLocationResolutionThen[A](verb: String, resolved: Option[SkillLocation])(
    f: Option[SkillLocation] => A
  ): A = {
    resolved match {
      case Some(SkillLocation.Project) =>
        println(s"No global skills found. $verb from project scope.".yellow)
      case Some(SkillLocation.Global) =>
        println(s"No project skills found. $verb from global scope.".yellow)
      case None => ()
    }
    f(resolved)
  }

  /** Report agent resolution result, then delegate to continuation.
    * If resolved (Some), prints an auto-select message and path labels before calling f.
    * If not resolved (None), calls f directly (caller handles prompting).
    */
  def reportAgentResolutionThen[A](
    resolved: Option[Agent],
    skillsInScope: List[Skill],
  )(f: Option[Agent] => A): A = {
    resolved.foreach { agent =>
      println(s"Only ${agent.toString} has skills. Auto-selecting ${agent.toString}.".yellow)
      val agentLocations = skillsInScope.filter(_.agent === agent).map(_.location).distinct.sortBy(_.ordinal)
      agentLocationLabels(agent, agentLocations).foreach(println)
    }
    f(resolved)
  }
}
