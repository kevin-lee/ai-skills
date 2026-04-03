package aiskills.cli.commands

import aiskills.cli.CliDefaults
import aiskills.core.utils.{AgentsMd, Dirs, Skills}
import aiskills.core.{Agent, RemoveOptions, Skill, SkillLocation}
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*

object Remove {

  /** Interactively manage (remove) installed skills via multi-select prompt. */
  def removeInteractive(): Unit = {
    val allSkills = Skills.findAllSkills()

    if allSkills.isEmpty then println("No skills installed.")
    else {
      val hasProjectSkills = allSkills.exists(_.location === SkillLocation.Project)

      val locationsResult: Either[Int, List[SkillLocation]] =
        if hasProjectSkills then promptForScope()
        else {
          println("No project skills found. Removing from global scope.".yellow)
          List(SkillLocation.Global).asRight
        }

      locationsResult match {
        case Left(code) => sys.exit(code)
        case Right(locations) =>
          val skillsInScope = allSkills.filter(s => locations.contains(s.location))

          if skillsInScope.isEmpty then {
            val scopeLabel = locations.map(_.toString.toLowerCase).mkString(" and ")
            println(s"No skills found in $scopeLabel scope.".yellow)
          } else {
            val agentsWithCounts = Agent.all.flatMap { agent =>
              val count = skillsInScope.count(_.agent === agent)
              if count > 0 then (agent, count).some else none
            }

            promptForAgents(agentsWithCounts) match {
              case Left(code) => sys.exit(code)
              case Right(selectedAgents) =>
                if selectedAgents.isEmpty then println("No agents selected.".yellow)
                else {
                  val filtered = skillsInScope.filter(s => selectedAgents.contains(s.agent))
                  if filtered.isEmpty then println("No skills found for the selected agents.".yellow)
                  else {
                    promptForSkillsAndRemove(filtered) match {
                      case Left(code) => sys.exit(code)
                      case Right(()) => ()
                    }
                  }
                }
            }
          }
      }
    }
  }

  private def promptForSkillsAndRemove(skills: List[Skill]): Either[Int, Unit] = {
    val sorted = skills.sortBy { s =>
      (s.agent.ordinal, if s.location === SkillLocation.Project then 0 else 1, s.name)
    }

    val labels = sorted.map { skill =>
      val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
      val locationLabel = s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel"
      s"${skill.name.padTo(25, ' ')} $locationLabel"
    }

    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.multiChoiceNoneSelected("Select skills to remove", labels, CliDefaults.multiChoiceModify) match {
        case Completion.Finished(selectedLabels) =>
          if selectedLabels.isEmpty then println("No skills selected for removal.".yellow)
          else {
            val selectedIndices = selectedLabels.flatMap { label =>
              labels.zipWithIndex.find { case (l, _) => l === label }.map { case (_, idx) => idx }
            }
            val removedSkills   = selectedIndices.map(sorted(_))
            for skill <- removedSkills do {
              os.remove.all(skill.path)
              val pathLabel = Dirs.displaySkillsDir(skill.agent, skill.location)
              println(
                s"\u2705 Removed: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel".green
              )
            }

            val agentLocationPairs = removedSkills.map(s => (s.agent, s.location)).distinct
            for (agent, location) <- agentLocationPairs do AgentsMd.updateAgentsMdForAgent(agent, location)

            println(s"\n\u2705 Removed ${selectedIndices.length} skill(s)".green)
          }
          ().asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def promptForScope(): Either[Int, List[SkillLocation]] = {
    val options = List("project", "global", "both")
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice("Select scope", options) match {
        case Completion.Finished(selected) =>
          selected match {
            case "project" => List(SkillLocation.Project).asRight
            case "global" => List(SkillLocation.Global).asRight
            case _ => List(SkillLocation.Project, SkillLocation.Global).asRight
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

  private def promptForAgents(agentsWithCounts: List[(Agent, Int)]): Either[Int, List[Agent]] = {
    val labels = agentsWithCounts.map { (agent, count) =>
      s"${agent.toString.padTo(15, ' ')} ($count skill(s))"
    }
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.multiChoiceNoneSelected("Select agent(s)", labels, CliDefaults.multiChoiceModify) match {
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

  /** Remove a specific skill by name from the specified agent(s) and location(s). */
  def removeSkill(skillName: String, options: RemoveOptions): Unit = {
    val locations: List[SkillLocation] = options.locations.toList

    val agents = options.agent.getOrElse(Nil)

    val targets = for {
      agent    <- agents
      location <- locations
    } yield (agent, location)

    val removed  = List.newBuilder[(aiskills.core.Agent, SkillLocation)]
    val notFound = List.newBuilder[(aiskills.core.Agent, SkillLocation)]

    for (agent, location) <- targets do {
      val skillDir = Dirs.getSkillsDir(agent, location) / skillName
      if os.exists(skillDir / "SKILL.md") then {
        os.remove.all(skillDir)
        val pathLabel = Dirs.displaySkillsDir(agent, location)
        println(
          s"\u2705 Removed: $skillName (${location.toString.toLowerCase}, ${agent.toString}): $pathLabel".green
        )
        removed += agent -> location
      } else {
        notFound += agent -> location
      }
    }

    val removedResult  = removed.result()
    val notFoundResult = notFound.result()

    val agentLocationPairs = removedResult.distinct
    for (agent, location) <- agentLocationPairs do AgentsMd.updateAgentsMdForAgent(agent, location)

    if removedResult.isEmpty then {
      val targetDescs = notFoundResult
        .map { (agent, location) =>
          s"${location.toString.toLowerCase}/${agent.toString}"
        }
        .mkString(", ")
      System.err.println(s"Error: Skill '$skillName' not found in: $targetDescs")
      sys.exit(1)
    } else if notFoundResult.nonEmpty then {
      for (agent, location) <- notFoundResult do {
        val pathLabel = Dirs.displaySkillsDir(agent, location)
        System
          .err
          .println(
            s"Warning: Skill '$skillName' not found in ${location.toString.toLowerCase}/${agent.toString}: $pathLabel"
          )
      }
    } else ()
  }
}
