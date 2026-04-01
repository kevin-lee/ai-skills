package aiskills.cli.commands

import aiskills.core.{Agent, Skill, SkillLocation, SyncOptions}
import aiskills.core.utils.{AgentsMd, Dirs, Skills, TerminalWidth}
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*

import OverwritePrompt.{BulkDecision, OverwriteChoice}

object Sync {

  /** Sync skills between agent directories. */
  def syncSkills(options: SyncOptions): Unit =
    (options.from, options.to) match {
      // Interactive mode: no flags provided
      case (None, None) =>
        interactiveSync(options.yes)

      // Non-interactive: from/to specified
      case (Some((sourceLocation, from)), Some(targets)) =>
        val targetLocations = options.targetLocations.toList

        if options.skillNames.nonEmpty then {
          // Specific skill(s)
          val sourceSkills = Skills.findSkillsByAgent(from, sourceLocation)

          val (notFound, found) = options.skillNames.partitionMap { name =>
            sourceSkills.find(_.name === name).toRight(name)
          }

          if notFound.nonEmpty then {
            System
              .err
              .println(
                s"Error: Skill(s) not found in ${from.toString} (${sourceLocation.toString.toLowerCase}): ${notFound.mkString(", ")}".red
              )
            sys.exit(1)
          }

          for target <- targets.filterNot(_ === from) do {
            syncSelectedSkillsWithLocations(found, from, target, sourceLocation, targetLocations, options.yes)
          }
        } else {
          // All skills
          for target <- targets.filterNot(_ === from) do {
            syncAllSkillsWithLocations(from, target, sourceLocation, targetLocations, options.yes)
          }
        }

      case _ =>
        System.err.println("Error: Invalid flag combination.".red)
        System.err.println("Usage:")
        System.err.println("")
        System.err.println("  <location> should be either 'project' or 'global'")
        System.err.println("")
        System
          .err
          .println(
            "  aiskills sync                                                                    # Interactive"
          )
        System
          .err
          .println(
            "  aiskills sync <skill> [<skill>...] --from <location>:<agent> --to <agent> --project    # Specific skill(s)"
          )
        System
          .err
          .println(
            "  aiskills sync --from <location>:<agent> --to <agent> --project                   # All skills"
          )
        System
          .err
          .println(
            "  aiskills sync --from <location>:<agent> --to all --project --global              # All skills to all"
          )
        sys.exit(1)
    }

  private def syncAllSkillsWithLocations(
    from: Agent,
    to: Agent,
    sourceLocation: SkillLocation,
    targetLocations: List[SkillLocation],
    yes: Boolean,
  ): Unit = {
    if from === to then println(s"Skipped: source and target are the same agent (${from.toString})".yellow)
    else {
      val sourceSkills = Skills.findSkillsByAgent(from, sourceLocation)

      if sourceSkills.isEmpty then println(
        s"No skills found in ${from.toString} (${sourceLocation.toString.toLowerCase}).".yellow
      )
      else {
        val targetLabel = targetLocations.map(_.toString.toLowerCase).mkString(" and ")
        println(
          s"Syncing ${sourceSkills.length} skill(s) from ${from.toString} (${sourceLocation.toString.toLowerCase}) to ${to.toString} ($targetLabel)...".dim
        )

        val (synced, _) =
          sourceSkills.foldLeft((0, BulkDecision.Undecided: BulkDecision)) {
            case ((count, bulk), s) =>
              targetLocations.foldLeft((count, bulk)) {
                case ((count, bulk), targetLocation) =>
                  val targetDir  = Dirs.getSkillsDir(to, targetLocation)
                  val targetPath = targetDir / s.name

                  def doSync(): Int = {
                    if os.exists(targetPath) then {
                      println(s"Overwriting: ${s.name} (all existing files and folders will be removed)".dim)
                      os.remove.all(targetPath)
                    } else ()
                    os.makeDir.all(targetDir)
                    os.copy(s.path, targetPath, replaceExisting = true)
                    println(
                      s"\u2705 Synced: ${s.name} -> ${to.toString} (${targetLocation.toString.toLowerCase})".green
                    )
                    count + 1
                  }

                  if !os.exists(targetPath) then {
                    os.makeDir.all(targetDir)
                    os.copy(s.path, targetPath, replaceExisting = true)
                    println(
                      s"\u2705 Synced: ${s.name} -> ${to.toString} (${targetLocation.toString.toLowerCase})".green
                    )
                    (count + 1, bulk)
                  } else if yes then (doSync(), bulk)
                  else
                    bulk match {
                      case BulkDecision.OverwriteAll =>
                        (doSync(), bulk)

                      case BulkDecision.SkipAll =>
                        println(s"Skipped: ${s.name}".yellow)
                        (count, bulk)

                      case BulkDecision.Undecided =>
                        val pathLabel = Dirs.displaySkillsDir(to, targetLocation)
                        OverwritePrompt.askOverwriteChoice(
                          s.name,
                          s"Skill '${s.name}' already exists in ${to.toString} (${targetLocation.toString.toLowerCase}): $pathLabel. What would you like to do?",
                        ) match {
                          case Left(code) => sys.exit(code)

                          case Right(OverwriteChoice.Yes) =>
                            (doSync(), BulkDecision.Undecided)

                          case Right(OverwriteChoice.No) =>
                            println(s"Skipped: ${s.name}".yellow)
                            (count, BulkDecision.Undecided)

                          case Right(OverwriteChoice.YesToAll) =>
                            (doSync(), BulkDecision.OverwriteAll)

                          case Right(OverwriteChoice.NoToAll) =>
                            println(s"Skipped: ${s.name}".yellow)
                            (count, BulkDecision.SkipAll)
                        }
                    }
              }
          }

        println(
          s"\n\u2705 Sync complete: $synced skill(s) synced from ${from.toString} to ${to.toString}".green
        )

        for targetLocation <- targetLocations do {
          AgentsMd.updateAgentsMdForAgent(to, targetLocation)
        }
      }
    }
  }

  private def syncSelectedSkillsWithLocations(
    selectedSkills: List[Skill],
    from: Agent,
    to: Agent,
    sourceLocation: SkillLocation,
    targetLocations: List[SkillLocation],
    yes: Boolean,
  ): Unit = {
    if from === to then println(s"Skipped: source and target are the same agent (${from.toString})".yellow)
    else if selectedSkills.isEmpty then println("No skills selected.".yellow)
    else {
      val targetLabel = targetLocations.map(_.toString.toLowerCase).mkString(" and ")
      println(
        s"Syncing ${selectedSkills.length} skill(s) from ${from.toString} (${sourceLocation.toString.toLowerCase}) to ${to.toString} ($targetLabel)...".dim
      )

      val (synced, _) =
        selectedSkills.foldLeft((0, BulkDecision.Undecided: BulkDecision)) {
          case ((count, bulk), s) =>
            targetLocations.foldLeft((count, bulk)) {
              case ((count, bulk), targetLocation) =>
                val targetDir  = Dirs.getSkillsDir(to, targetLocation)
                val targetPath = targetDir / s.name

                def doSync(): Int = {
                  if os.exists(targetPath) then {
                    println(s"Overwriting: ${s.name} (all existing files and folders will be removed)".dim)
                    os.remove.all(targetPath)
                  } else ()
                  os.makeDir.all(targetDir)
                  os.copy(s.path, targetPath, replaceExisting = true)
                  println(
                    s"\u2705 Synced: ${s.name} -> ${to.toString} (${targetLocation.toString.toLowerCase})".green
                  )
                  count + 1
                }

                if !os.exists(targetPath) then {
                  os.makeDir.all(targetDir)
                  os.copy(s.path, targetPath, replaceExisting = true)
                  println(
                    s"\u2705 Synced: ${s.name} -> ${to.toString} (${targetLocation.toString.toLowerCase})".green
                  )
                  (count + 1, bulk)
                } else if yes then (doSync(), bulk)
                else
                  bulk match {
                    case BulkDecision.OverwriteAll =>
                      (doSync(), bulk)

                    case BulkDecision.SkipAll =>
                      println(s"Skipped: ${s.name}".yellow)
                      (count, bulk)

                    case BulkDecision.Undecided =>
                      val pathLabel = Dirs.displaySkillsDir(to, targetLocation)
                      OverwritePrompt.askOverwriteChoice(
                        s.name,
                        s"Skill '${s.name}' already exists in ${to.toString} (${targetLocation.toString.toLowerCase}): $pathLabel. What would you like to do?",
                      ) match {
                        case Left(code) => sys.exit(code)

                        case Right(OverwriteChoice.Yes) =>
                          (doSync(), BulkDecision.Undecided)

                        case Right(OverwriteChoice.No) =>
                          println(s"Skipped: ${s.name}".yellow)
                          (count, BulkDecision.Undecided)

                        case Right(OverwriteChoice.YesToAll) =>
                          (doSync(), BulkDecision.OverwriteAll)

                        case Right(OverwriteChoice.NoToAll) =>
                          println(s"Skipped: ${s.name}".yellow)
                          (count, BulkDecision.SkipAll)
                      }
                  }
            }
        }

      println(
        s"\n\u2705 Sync complete: $synced skill(s) synced from ${from.toString} to ${to.toString}".green
      )

      for targetLocation <- targetLocations do {
        AgentsMd.updateAgentsMdForAgent(to, targetLocation)
      }
    }
  }

  private def interactiveSync(yes: Boolean): Unit = {
    val allSkills = Skills.findAllSkills()

    if allSkills.isEmpty then {
      println("No skills installed. Install skills first:")
      println(s"  ${"aiskills install anthropics/skills".cyan}")
    } else {
      // Step 1: Pick source location
      val sourceLocation = {
        val options = List("project", "global")
        aiskills.cli.SigintHandler.install()
        val result  = Prompts.sync.use { prompts =>
          prompts.singleChoice("Select source location", options) match {
            case Completion.Finished(selected) =>
              selected match {
                case "project" => Right(SkillLocation.Project)
                case _ => Right(SkillLocation.Global)
              }
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
          case Right(v) => v
        }
      }

      // Step 2: Pick source agent (filtered by source location)
      val skillsByAgent = allSkills.filter(_.location === sourceLocation).groupBy(_.agent)
      val agents        = Agent.all.filter(skillsByAgent.contains)

      if agents.isEmpty then println(s"No skills found in ${sourceLocation.toString.toLowerCase} scope.".yellow)
      else {
        val agentLabels = agents.map { a =>
          val count = skillsByAgent(a).length
          s"${a.toString.padTo(15, ' ')} ($count skill(s))"
        }

        val sourceAgent = {
          aiskills.cli.SigintHandler.install()
          val result = Prompts.sync.use { prompts =>
            prompts.singleChoice("Select source agent", agentLabels) match {
              case Completion.Finished(selectedLabel) =>
                Right(agents.find(a => selectedLabel.contains(a.toString)))
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
            case Right(v) => v
          }
        }

        sourceAgent match {
          case None =>
            println("No agent selected.".yellow)
          case Some(from) =>
            // Step 3: Pick skills to sync
            val sourceSkills = Skills.findSkillsByAgent(from, sourceLocation).sortBy(_.name)

            val terminalWidth  = TerminalWidth.getTerminalWidth()
            val cue4sPrefixLen = 4 // " ◉ "
            val namePadLen     = 25
            val maxDescLen     =
              (terminalWidth - cue4sPrefixLen - namePadLen - 1).max(10) // -1 for space between name and desc
            val skillLabels    = sourceSkills.map { s =>
              val desc =
                if s.description.length > maxDescLen then s.description.take(maxDescLen - 3) + "..."
                else s.description
              s"${s.name.padTo(namePadLen, ' ')} $desc"
            }

            val selectedSkills = {
              aiskills.cli.SigintHandler.install()
              val result = Prompts.sync.use { prompts =>
                prompts.multiChoiceAllSelected("Select skills to sync", skillLabels) match {
                  case Completion.Finished(selectedLabels) =>
                    Right(sourceSkills.filter(s => selectedLabels.exists(_.startsWith(s.name))))
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
                case Right(v) => v
              }
            }

            if selectedSkills.isEmpty then println("No skills selected.".yellow)
            else {
              // Step 4: Pick target location
              val targetLocations = {
                val options = List("project", "global", "both")
                aiskills.cli.SigintHandler.install()
                val result  = Prompts.sync.use { prompts =>
                  prompts.singleChoice("Select target location", options) match {
                    case Completion.Finished(selected) =>
                      selected match {
                        case "project" => Right(List(SkillLocation.Project))
                        case "global" => Right(List(SkillLocation.Global))
                        case _ => Right(List(SkillLocation.Project, SkillLocation.Global))
                      }
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
                  case Right(v) => v
                }
              }

              // Step 5: Pick target agents
              val targetAgents = Agent.all.filterNot(_ === from)
              val targetLabels = targetAgents.map(_.toString)

              val selectedTargets = {
                aiskills.cli.SigintHandler.install()
                val result = Prompts.sync.use { prompts =>
                  prompts.multiChoiceNoneSelected("Select target agent(s)", targetLabels) match {
                    case Completion.Finished(selectedLabels) =>
                      Right(targetAgents.filter(a => selectedLabels.contains(a.toString)))
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
                  case Right(v) => v
                }
              }

              if selectedTargets.isEmpty then println("No target agents selected.".yellow)
              else
                for target <- selectedTargets do {
                  syncSelectedSkillsWithLocations(selectedSkills, from, target, sourceLocation, targetLocations, yes)
                }
            }
        }
      }
    }
  }
}
