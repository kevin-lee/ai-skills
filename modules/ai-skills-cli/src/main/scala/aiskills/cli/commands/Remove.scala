package aiskills.cli.commands

import aiskills.core.{RemoveOptions, SkillLocation}
import aiskills.core.utils.{AgentsMd, Dirs, Skills}
import cats.syntax.all.*
import extras.scala.io.syntax.color.*
import cue4s.*

object Remove {

  /** Interactively manage (remove) installed skills via multi-select prompt. */
  def removeInteractive(): Unit = {
    val skills = Skills.findAllSkills()

    if skills.isEmpty then println("No skills installed.")
    else {
      val sorted = skills.sortBy { s =>
        (s.agent.ordinal, if s.location === SkillLocation.Project then 0 else 1, s.name)
      }

      val labels = sorted.map { skill =>
        val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
        val locationLabel = s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel"
        s"${skill.name.padTo(25, ' ')} $locationLabel"
      }

      aiskills.cli.SigintHandler.install()
      val result = Prompts.sync.use { prompts =>
        prompts.multiChoiceNoneSelected("Select skills to remove", labels) match {
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
      result match {
        case Left(code) => sys.exit(code)
        case Right(()) => ()
      }
    }
  }

  /** Remove a specific skill by name from the specified agent(s) and location(s). */
  def removeSkill(skillName: String, options: RemoveOptions): Unit = {
    val locations: List[SkillLocation] =
      List.concat(
        if options.project then List(SkillLocation.Project) else Nil,
        if options.global then List(SkillLocation.Global) else Nil,
      )

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
        removed += ((agent, location))
      } else {
        notFound += ((agent, location))
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
