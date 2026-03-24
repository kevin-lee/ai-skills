package aiskills.cli.commands

import aiskills.core.{Agent, SkillLocation, SyncOptions}
import aiskills.core.utils.{AgentsMd, Dirs, Skills}
import cue4s.*
import extras.scala.io.syntax.color.*

object Sync {

  /** Sync skills between agent directories. */
  def syncSkills(options: SyncOptions): Unit =
    (options.from, options.to, options.skillName, options.allAgents) match {
      // Interactive mode: no flags provided
      case (None, None, None, false) =>
        interactiveSync(options.yes)

      // Specific skill, from/to specified
      case (Some(from), Some(to), Some(name), false) =>
        syncSingleSkill(name, from, to, options.yes)

      // Specific skill, from specified, all agents
      case (Some(from), None, Some(name), true) =>
        val targets = Agent.all.filterNot(_ == from)
        for target <- targets do {
          syncSingleSkill(name, from, target, options.yes)
        }

      // All skills from one agent to another
      case (Some(from), Some(to), None, false) =>
        syncAllSkills(from, to, options.yes)

      // All skills from one agent to all others
      case (Some(from), None, None, true) =>
        val targets = Agent.all.filterNot(_ == from)
        for target <- targets do {
          syncAllSkills(from, target, options.yes)
        }

      case _ =>
        System.err.println("Error: Invalid flag combination.".red)
        System.err.println("Usage:")
        System.err.println("  aiskills sync                                            # Interactive")
        System.err.println("  aiskills sync <skill> --from <agent> --to <agent>        # One skill")
        System.err.println("  aiskills sync <skill> --from <agent> --all-agents        # One skill to all")
        System.err.println("  aiskills sync --from <agent> --to <agent>                # All skills")
        System.err.println("  aiskills sync --from <agent> --all-agents                # All skills to all")
        sys.exit(1)
    }

  private def syncSingleSkill(name: String, from: Agent, to: Agent, yes: Boolean): Unit = {
    if from == to then println(s"Skipped: source and target are the same agent (${from.toString})".yellow)
    else {
      // Try project first, then global
      val sourceSkills = Skills.findSkillsByAgent(from, global = false) ++ Skills.findSkillsByAgent(from, global = true)
      val skill        = sourceSkills.find(_.name == name)

      skill match {
        case None =>
          System.err.println(s"Error: Skill '$name' not found in ${from.toString} directories".red)
          sys.exit(1)

        case Some(s) =>
          val isGlobal   = s.location == SkillLocation.Global
          val targetDir  = Dirs.getSkillsDir(to, global = isGlobal)
          val targetPath = targetDir / name

          val proceed =
            if os.exists(targetPath) && !yes then Prompts.sync.use { prompts =>
              println(
                s"\u26a0 All existing files and folders in '$name' will be removed if you choose to overwrite.".yellow
              )
              prompts.confirm(
                s"Skill '$name' already exists in ${to.toString} (${s.location.toString.toLowerCase}). Overwrite?".yellow,
                default = false,
              ) match {
                case Completion.Finished(v) =>
                  if v then os.remove.all(targetPath) else ()
                  v
                case Completion.Fail(CompletionError.Interrupted) =>
                  println("\n\nCancelled by user".yellow)
                  sys.exit(0)
                case Completion.Fail(CompletionError.Error(_)) => false
              }
            }
            else if os.exists(targetPath) then {
              println(s"Overwriting: $name (all existing files and folders will be removed)".dim)
              os.remove.all(targetPath)
              true
            } else
              true

          if proceed then {
            os.makeDir.all(targetDir)
            os.copy(s.path, targetPath, replaceExisting = true)
            println(s"\u2705 Synced: $name (${from.toString} -> ${to.toString})".green)

            AgentsMd.updateAgentsMdForAgent(to, isGlobal)
          } else
            println(s"Skipped: $name".yellow)
      }
    }
  }

  private def syncAllSkills(from: Agent, to: Agent, yes: Boolean): Unit = {
    if from == to then println(s"Skipped: source and target are the same agent (${from.toString})".yellow)
    else {
      val sourceSkills =
        Skills.findSkillsByAgent(from, global = false) ++ Skills.findSkillsByAgent(from, global = true)

      if sourceSkills.isEmpty then println(s"No skills found in ${from.toString} directories.".yellow)
      else {
        println(s"Syncing ${sourceSkills.length} skill(s) from ${from.toString} to ${to.toString}...".dim)

        val synced = sourceSkills.count { s =>
          val isGlobal   = s.location == SkillLocation.Global
          val targetDir  = Dirs.getSkillsDir(to, global = isGlobal)
          val targetPath = targetDir / s.name

          if os.exists(targetPath) && !yes then {
            println(s"Skipped: ${s.name} (already exists in ${to.toString}, use --yes to overwrite)".dim)
            false
          } else {
            if os.exists(targetPath) then {
              println(s"Overwriting: ${s.name} (all existing files and folders will be removed)".dim)
              os.remove.all(targetPath)
            } else ()
            os.makeDir.all(targetDir)
            os.copy(s.path, targetPath, replaceExisting = true)
            println(s"\u2705 Synced: ${s.name}".green)
            true
          }
        }

        println(s"\n\u2705 Sync complete: $synced skill(s) synced from ${from.toString} to ${to.toString}".green)

        // Update AGENTS.md for target if needed
        AgentsMd.updateAgentsMdForAgent(to, global = false)
        AgentsMd.updateAgentsMdForAgent(to, global = true)
      }
    }
  }

  private def interactiveSync(yes: Boolean): Unit = {
    val allSkills = Skills.findAllSkills()

    if allSkills.isEmpty then {
      println("No skills installed. Install skills first:")
      println(s"  ${"aiskills install anthropics/skills".cyan}")
    } else {
      val grouped = allSkills.groupBy(_.agent)
      val agents  = Agent.all.filter(grouped.contains)

      if agents.length < 1 then println("No skills found in any agent directory.".yellow)
      else {
        // Step 1: Pick source agent
        val agentLabels = agents.map { a =>
          val count = grouped(a).length
          s"${a.toString.padTo(15, ' ')} ($count skill(s))"
        }

        val sourceAgent = Prompts.sync.use { prompts =>
          prompts.multiChoiceNoneSelected("Select source agent (pick one)", agentLabels) match {
            case Completion.Finished(selectedLabels) =>
              selectedLabels.headOption.flatMap { label =>
                agents.find(a => label.contains(a.toString))
              }
            case Completion.Fail(CompletionError.Interrupted) =>
              println("\n\nCancelled by user".yellow)
              sys.exit(0)
            case Completion.Fail(CompletionError.Error(msg)) =>
              System.err.println(s"Error: $msg")
              sys.exit(1)
          }
        }

        sourceAgent match {
          case None =>
            println("No agent selected.".yellow)
          case Some(from) =>
            // Step 2: Pick target agents
            val targetAgents = Agent.all.filterNot(_ == from)
            val targetLabels = targetAgents.map(_.toString)

            val selectedTargets = Prompts.sync.use { prompts =>
              prompts.multiChoiceNoneSelected("Select target agent(s)", targetLabels) match {
                case Completion.Finished(selectedLabels) =>
                  targetAgents.filter(a => selectedLabels.contains(a.toString))
                case Completion.Fail(CompletionError.Interrupted) =>
                  println("\n\nCancelled by user".yellow)
                  sys.exit(0)
                case Completion.Fail(CompletionError.Error(msg)) =>
                  System.err.println(s"Error: $msg")
                  sys.exit(1)
              }
            }

            if selectedTargets.isEmpty then println("No target agents selected.".yellow)
            else
              for target <- selectedTargets do {
                syncAllSkills(from, target, yes)
              }
        }
      }
    }
  }
}
