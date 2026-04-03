package aiskills.cli.commands

import aiskills.cli.CliDefaults
import aiskills.core.utils.{Dirs, SkillNames, Skills, TerminalWidth}
import aiskills.core.{Agent, ReadOptions, Skill, SkillLocation, SkillLocationInfo}
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*

object Read {

  private def generateSeparator(separatorChar: Char) =
    "\n" + (separatorChar.toString * TerminalWidth.getTerminalWidth()).green.bold + "\n"

  private val separator = generateSeparator('=')

  /** Read skill(s) to stdout — non-interactive mode (with flags). */
  def readSkill(skillNames: List[String], options: ReadOptions): Unit = {
    val names = SkillNames.normalizeSkillNames(skillNames)
    if names.isEmpty then {
      System.err.println("Error: No skill names provided")
      sys.exit(1)
    } else {
      val locations = options.locations.toList
      val agents    = options.agent.getOrElse(Nil)

      val resolved = List.newBuilder[(String, SkillLocationInfo)]
      val missing  = List.newBuilder[String]

      for name <- names do {
        val found = (for {
          agent    <- agents
          location <- locations
          skillPath = Dirs.getSkillsDir(agent, location) / name / "SKILL.md"
          if os.exists(skillPath)
        } yield SkillLocationInfo(
          path = skillPath,
          baseDir = skillPath / os.up,
          source = skillPath / os.up / os.up,
          agent = agent,
          location = location,
        ))

        if found.isEmpty then missing += name
        else found.foreach(skill => resolved += name -> skill)
      }

      val missingList = missing.result()
      if missingList.nonEmpty then {
        System.err.println(s"Error: Skill(s) not found: ${missingList.mkString(", ")}")
        System.err.println()
        System.err.println("Searched:")
        for {
          agent    <- agents
          location <- locations
        } do {
          val dir   = Dirs.getSkillsDir(agent, location)
          val scope = location.toString.toLowerCase
          val label =
            if dir.startsWith(os.home) then dir.toString.replace(os.home.toString, "~")
            else dir.relativeTo(os.pwd).toString
          System.err.println(s"  $label ($scope, ${agent.toString})")
        }
        System.err.println()
        System.err.println("Install skills: aiskills install owner/repo")
        sys.exit(1)
      } else ()

      val resolvedList = resolved.result()
      for ((name, skill), idx) <- resolvedList.zipWithIndex do {
        if idx > 0 then println(separator)
        val content = os.read(skill.path)
        println("       Reading:".bold + s" ${name.blue.bold}")
        println("Base directory:".bold + s" ${skill.baseDir.toString.yellow.bold}")

        println()
        println(content)
        println()
        println("Skill read:".bold + s" ${name.blue.bold}")
      }
    }
  }

  /** Interactive mode: prompt for location -> agents -> skills -> read selected. */
  def readInteractive(): Unit = {
    val allSkills = Skills.findAllSkills()

    if allSkills.isEmpty then {
      println("No skills installed.\n")
      println("Install skills:")
      println(s"  ${"aiskills install anthropics/skills".cyan}   ${"# Install from GitHub".dim}")
    } else {
      promptForScope() match {
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

            promptForAgents(agentsWithCounts) match {
              case Left(code) => sys.exit(code)
              case Right(selectedAgents) =>
                if selectedAgents.isEmpty then println("No agents selected.".yellow)
                else {
                  val filtered = skillsInScope.filter(s => selectedAgents.contains(s.agent))
                  if filtered.isEmpty then println("No skills found for the selected agents.".yellow)
                  else {
                    promptForSkills(filtered) match {
                      case Left(code) => sys.exit(code)
                      case Right(selectedSkills) =>
                        if selectedSkills.isEmpty then println("No skills selected.".yellow)
                        else {
                          for (skill, idx) <- selectedSkills.zipWithIndex do {
                            if idx > 0 then println(separator)
                            val skillPath = skill.path / "SKILL.md"
                            val content   = os.read(skillPath)
                            println("       Reading:".bold + s" ${skill.name.blue.bold}")
                            println("Base directory:".bold + s" ${skill.path.toString.yellow.bold}")
                            println()
                            println(content)
                            println()
                            println("Skill read:".bold + s" ${skill.name.blue.bold}")
                          }
                        }
                    }
                  }
                }
            }
          }
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

  private def promptForSkills(skills: List[Skill]): Either[Int, List[Skill]] = {
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
      prompts.multiChoiceNoneSelected("Select skill(s) to read", labels, CliDefaults.multiChoiceModify) match {
        case Completion.Finished(selectedLabels) =>
          val selectedIndices = selectedLabels.flatMap { label =>
            labels.zipWithIndex.find { case (l, _) => l === label }.map { case (_, idx) => idx }
          }
          selectedIndices.map(sorted(_)).asRight
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }
}
