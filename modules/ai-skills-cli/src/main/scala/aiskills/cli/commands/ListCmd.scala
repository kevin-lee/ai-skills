package aiskills.cli.commands

import aiskills.cli.CliDefaults
import aiskills.core.utils.{Dirs, Skills}
import aiskills.core.{Agent, ListOptions, Skill, SkillLocation}
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*

object ListCmd {

  /** List installed skills, optionally filtered by scope and agent. */
  def listSkills(options: ListOptions): Unit = {
    val hasAnyFlag = options.locations.nonEmpty || options.agent.isDefined
    if hasAnyFlag then listWithFlags(options)
    else listInteractive()
  }

  private def listWithFlags(options: ListOptions): Unit = {
    val locations: List[SkillLocation] = options.locations.toList

    val agents: List[Agent] =
      options.agent.getOrElse(Agent.all)

    val skills = for {
      agent    <- agents
      location <- locations
      skill    <- Skills.findSkillsByAgent(agent, location)
    } yield skill

    if skills.isEmpty then printEmptyFiltered(agents, locations)
    else displaySkills(skills)
  }

  private def listInteractive(): Unit = {
    val allSkills = Skills.findAllSkills()

    if allSkills.isEmpty then {
      println("No skills installed.\n")
      println("Install skills:")
      println(
        s"  ${"aiskills install anthropics/skills".cyan}                   ${"# Project, universal (default)".dim}"
      )
      println(s"  ${"aiskills install owner/skill --agent claude".cyan}          ${"# Project, Claude".dim}")
      println(s"  ${"aiskills install owner/skill --agent all".cyan}              ${"# Project, all agents".dim}")
      println(s"  ${"aiskills install owner/skill --global".cyan}                ${"# Global".dim}")
    } else {
      val locationsResult: Either[Int, List[SkillLocation]] =
        InteractiveHelper.reportLocationResolutionThen("Listing", InteractiveHelper.resolveLocations(allSkills)) {
          case Some(location) => List(location).asRight
          case None => promptForScope()
        }
      locationsResult match {
        case Left(code) => sys.exit(code)
        case Right(locations) =>
          val skillsInScope = allSkills.filter(s => locations.contains(s.location))

          if skillsInScope.isEmpty then {
            val scopeLabel = locations.map(_.toString.toLowerCase).mkString(" and ")
            println(s"No skills found in $scopeLabel scope.".yellow)
          } else {
            val agentsWithCounts = Agent
              .all
              .flatMap { agent =>
                val count = skillsInScope.count(_.agent === agent)
                if count > 0 then (agent, count).some else none
              }

            val agentsResult: Either[Int, List[Agent]] =
              InteractiveHelper.reportAgentResolutionThen(
                InteractiveHelper.resolveAgents(agentsWithCounts),
                skillsInScope
              ) {
                case Some(agent) => List(agent).asRight
                case None => promptForAgents(agentsWithCounts, skillsInScope)
              }
            agentsResult match {
              case Left(code) => sys.exit(code)
              case Right(selectedAgents) =>
                if selectedAgents.isEmpty then println("No agents selected.".yellow)
                else {
                  val filtered = skillsInScope.filter(s => selectedAgents.contains(s.agent))
                  if filtered.isEmpty then println("No skills found for the selected agents.".yellow)
                  else displaySkills(filtered)
                }
            }
          }
      }
    }
  }

  private def promptForScope(): Either[Int, List[SkillLocation]] = {
    val options = List("global", "project", "both")
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice("Select scope", options) match {
        case Completion.Finished(selected) =>
          selected match {
            case "project" => List(SkillLocation.Project).asRight
            case "global" => List(SkillLocation.Global).asRight
            case _ => List(SkillLocation.Global, SkillLocation.Project).asRight
          }
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def promptForAgents(
    agentsWithCounts: List[(Agent, Int)],
    skillsInScope: List[Skill]
  ): Either[Int, List[Agent]] = {
    val labels = agentsWithCounts.map { (agent, count) =>
      InteractiveHelper.buildAgentLabel(agent, count, skillsInScope)
    }
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.run(CliDefaults.mandatoryMultiChoiceNoneSelected("Select agent(s)", labels)) match {
        case Completion.Finished(selectedLabels) =>
          val selected = agentsWithCounts
            .filter { (agent, _) =>
              selectedLabels.exists(_.contains(agent.toString))
            }
            .map { case (agent, _) => agent }
          selected.asRight
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def printEmptyFiltered(agents: List[Agent], locations: List[SkillLocation]): Unit =
    for {
      agent    <- agents
      location <- locations
    } do {
      val pathLabel  = Dirs.displaySkillsDir(agent, location)
      val scopeLabel = location.toString.toLowerCase
      println(s"No skills found for the given filter ($scopeLabel, ${agent.toString}): $pathLabel".yellow)
    }

  private def displaySkills(skills: List[Skill]): Unit = {
    println("Available Skills:\n".bold)

    given Ordering[SkillLocation] = SkillLocation.ordering.reverse

    val sorted = skills.sortBy(s => (s.agent.ordinal, s.location, s.name))

    sorted.foreach { skill =>
      val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
      val locationLabel =
        s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString})".blue + s": $pathLabel".dim
      println(s"  ${skill.name.padTo(25, ' ').bold} $locationLabel")
      SkillDisplay.renderInfoBlock(skill.path)
      println(s"    ${skill.description.dim}\n")
    }

    val byAgent      = skills.groupBy(_.agent)
    val agentSummary = Agent
      .all
      .filter(byAgent.contains)
      .map { a =>
        val count = byAgent(a).length
        s"${a.toString}: $count"
      }
      .mkString(", ")

    val projectCount = skills.count(_.location === SkillLocation.Project)
    val globalCount  = skills.count(_.location === SkillLocation.Global)

    println(s"Summary: $projectCount project, $globalCount global (${skills.length} total)".dim)
    if byAgent.size > 1 then println(s"  By agent: $agentSummary".dim)
    else ()
  }
}
