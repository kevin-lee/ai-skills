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
      val locationsResult: Either[Int, List[SkillLocation]] =
        InteractiveHelper.reportLocationResolutionThen("Removing", InteractiveHelper.resolveLocations(allSkills)) {
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
            val agentsWithCounts = Agent.all.flatMap { agent =>
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
    given Ordering[SkillLocation] = SkillLocation.ordering.reverse

    val sorted = skills.sortBy(s => (s.agent.ordinal, s.location, s.name))

    val labels = sorted.map { skill =>
      val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
      val locationLabel = s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel"
      s"${skill.name.padTo(25, ' ')} $locationLabel"
    }

    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.run(CliDefaults.mandatoryMultiChoiceNoneSelected("Select skills to remove", labels)) match {
        case Completion.Finished(selectedLabels) =>
          val selectedIndices = selectedLabels.flatMap { label =>
            labels.zipWithIndex.find { case (l, _) => l === label }.map { case (_, idx) => idx }
          }
          val selectedSkills  = selectedIndices.map(sorted(_))
          confirmRemoval(selectedSkills) match {
            case Right(true) =>
              for skill <- selectedSkills do {
                os.remove.all(skill.path)
                val pathLabel = Dirs.displaySkillsDir(skill.agent, skill.location)
                println(
                  s"\u2705 Removed: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel".green
                )
              }

              val agentLocationPairs = selectedSkills.map(s => (s.agent, s.location)).distinct
              for (agent, location) <- agentLocationPairs do AgentsMd.updateAgentsMdForAgent(agent, location)

              println(s"\n\u2705 Removed ${selectedIndices.length} skill(s)".green)
              ().asRight
            case Right(false) =>
              println("Cancelled.".yellow)
              ().asRight
            case Left(code) =>
              code.asLeft
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

  private def confirmRemoval(skills: List[Skill]): Either[Int, Boolean] = {
    println(s"\nThe following skill(s) will be removed:")
    for skill <- skills do {
      val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
      val locationColor =
        if skill.location === SkillLocation.Project then skill.location.toString.toLowerCase.blue
        else skill.location.toString.toLowerCase.yellow
      println(s"  - ${skill.name.bold} ($locationColor, ${skill.agent.toString.cyan.bold}): ${pathLabel.dim}")
    }
    println()
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.confirm("Are you sure?", default = false) match {
        case Completion.Finished(confirmed) =>
          confirmed.asRight
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def confirmRemovalNonInteractive(
    skillName: String,
    foundTargets: List[(Agent, SkillLocation)]
  ): Either[Int, Boolean] = {
    println(s"\nThe following skill(s) will be removed:")
    for (agent, location) <- foundTargets do {
      val pathLabel     = Dirs.displaySkillsDir(agent, location)
      val locationColor =
        if location === SkillLocation.Project then location.toString.toLowerCase.blue
        else location.toString.toLowerCase.yellow
      println(s"  - ${skillName.bold} ($locationColor, ${agent.toString.cyan.bold}): ${pathLabel.dim}")
    }
    println()
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.confirm("Are you sure?", default = false) match {
        case Completion.Finished(confirmed) =>
          confirmed.asRight
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

    val found    = List.newBuilder[(Agent, SkillLocation)]
    val notFound = List.newBuilder[(Agent, SkillLocation)]

    for (agent, location) <- targets do {
      val skillDir = Dirs.getSkillsDir(agent, location) / skillName
      if os.exists(skillDir / "SKILL.md")
      then found += agent    -> location
      else notFound += agent -> location
    }

    val foundResult    = found.result()
    val notFoundResult = notFound.result()

    if foundResult.isEmpty then {
      val targetDescs = notFoundResult
        .map { (agent, location) =>
          s"${location.toString.toLowerCase}/${agent.toString}"
        }
        .mkString(", ")
      System.err.println(s"Error: Skill '$skillName' not found in: $targetDescs")
      sys.exit(1)
    }

    val confirmed =
      if options.yes then { true }
      else {
        confirmRemovalNonInteractive(skillName, foundResult) match {
          case Right(c) => c
          case Left(code) => sys.exit(code)
        }
      }

    if confirmed then {
      for (agent, location) <- foundResult do {
        val skillDir  = Dirs.getSkillsDir(agent, location) / skillName
        os.remove.all(skillDir)
        val pathLabel = Dirs.displaySkillsDir(agent, location)
        println(
          s"\u2705 Removed: $skillName (${location.toString.toLowerCase}, ${agent.toString}): $pathLabel".green
        )
      }

      val agentLocationPairs = foundResult.distinct
      for (agent, location) <- agentLocationPairs do AgentsMd.updateAgentsMdForAgent(agent, location)

      if notFoundResult.nonEmpty then {
        for (agent, location) <- notFoundResult do {
          val pathLabel = Dirs.displaySkillsDir(agent, location)
          System
            .err
            .println(
              s"Warning: Skill '$skillName' not found in ${location.toString.toLowerCase}/${agent.toString}: $pathLabel"
            )
        }
      } else ()
    } else println("Cancelled.".yellow)
  }
}
