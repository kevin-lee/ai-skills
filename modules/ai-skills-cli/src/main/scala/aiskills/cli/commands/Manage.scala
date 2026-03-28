package aiskills.cli.commands

import aiskills.core.SkillLocation
import aiskills.core.utils.{AgentsMd, Skills}
import cats.syntax.all.*
import extras.scala.io.syntax.color.*
import cue4s.*

object Manage {

  /** Interactively manage (remove) installed skills. */
  def manageSkills(): Unit = {
    val skills = Skills.findAllSkills()

    if skills.isEmpty then println("No skills installed.")
    else {
      val sorted = skills.sortBy { s =>
        (s.agent.ordinal, if s.location === SkillLocation.Project then 0 else 1, s.name)
      }

      val labels = sorted.map { skill =>
        val locationLabel = s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString})"
        s"${skill.name.padTo(25, ' ')} $locationLabel"
      }

      aiskills.cli.SigintHandler.install()
      val result = Prompts.sync.use { prompts =>
        prompts.multiChoiceNoneSelected("Select skills to remove", labels) match {
          case Completion.Finished(selectedLabels) =>
            if selectedLabels.isEmpty then println("No skills selected for removal.".yellow)
            else {
              val selectedIndices = selectedLabels.flatMap { label =>
                labels.zipWithIndex.find(_._1 === label).map(_._2)
              }
              val removedSkills   = selectedIndices.map(sorted(_))
              for skill <- removedSkills do {
                os.remove.all(skill.path)
                println(
                  s"\u2705 Removed: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString})".green
                )
              }

              val agentLocationPairs = removedSkills.map(s => (s.agent, s.location)).distinct
              for (agent, location) <- agentLocationPairs do AgentsMd.updateAgentsMdForAgent(agent, location)

              println(s"\n\u2705 Removed ${selectedIndices.length} skill(s)".green)
            }
            Right(())

          case Completion.Fail(CompletionError.Interrupted) =>
            println("\n\nCancelled by user".yellow)
            Left(0)

          case Completion.Fail(CompletionError.Error(msg)) =>
            System.err.println(s"Error: $msg")
            Left(1)
        }
      }
      result match {
        case Left(code) => sys.exit(code)
        case Right(()) => ()
      }
    }
  }
}
