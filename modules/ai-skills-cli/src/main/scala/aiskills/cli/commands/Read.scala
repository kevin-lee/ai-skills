package aiskills.cli.commands

import aiskills.core.ReadOptions
import aiskills.core.utils.{Dirs, SkillNames, Skills}

object Read {

  /** Read skill(s) to stdout (for AI agents). */
  def readSkill(skillNames: List[String], options: ReadOptions): Unit = {
    val names = SkillNames.normalizeSkillNames(skillNames)
    if names.isEmpty then {
      System.err.println("Error: No skill names provided")
      sys.exit(1)
    } else {
      val resolved = List.newBuilder[(String, aiskills.core.SkillLocationInfo)]
      val missing  = List.newBuilder[String]

      for name <- names do {
        Skills.findSkill(name, options.prefer) match {
          case Some(skill) => resolved += ((name, skill))
          case None => missing += name
        }
      }

      val missingList = missing.result()
      if missingList.nonEmpty then {
        System.err.println(s"Error: Skill(s) not found: ${missingList.mkString(", ")}")
        System.err.println()
        System.err.println("Searched:")
        for (dir, agent, location) <- Dirs.getSearchDirs() do {
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

      for (name, skill) <- resolved.result() do {
        val content = os.read(skill.path)
        println(s"Reading: $name")
        println(s"Base directory: ${skill.baseDir}")
        println()
        println(content)
        println()
        println(s"Skill read: $name")

        // Note alternatives on stderr
        val allLocations = Skills.findAllSkillLocations(name)
        if allLocations.length > 1 then {
          val otherDirs = allLocations
            .filterNot(_._1 == skill.source)
            .map {
              case (dir, agent, _) =>
                val label =
                  if dir.startsWith(os.home) then dir.toString.replace(os.home.toString, "~")
                  else dir.relativeTo(os.pwd).toString
                s"$label/ (${agent.toString.toLowerCase})"
            }
          if otherDirs.nonEmpty then System
            .err
            .println(s"Note: '$name' also found in: ${otherDirs.mkString(", ")} (use --prefer <agent>)")
          else ()
        } else ()
      }
    }
  }
}
