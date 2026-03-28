package aiskills.cli.commands

import aiskills.core.{Agent, SkillLocation}
import aiskills.core.utils.{Dirs, Skills}
import cats.syntax.all.*
import extras.scala.io.syntax.color.*

object ListCmd {

  /** List all installed skills. */
  def listSkills(): Unit = {
    println("Available Skills:\n".bold)

    val skills = Skills.findAllSkills()

    if skills.isEmpty then {
      println("No skills installed.\n")
      println("Install skills:")
      println(
        s"  ${"aiskills install anthropics/skills".cyan}                   ${"# Project, universal (default)".dim}"
      )
      println(s"  ${"aiskills install owner/skill --agent claude".cyan}          ${"# Project, Claude".dim}")
      println(s"  ${"aiskills install owner/skill --all-agents".cyan}            ${"# Project, all agents".dim}")
      println(s"  ${"aiskills install owner/skill --global".cyan}                ${"# Global".dim}")
    } else {
      val sorted = skills.sortBy { s =>
        (s.agent.ordinal, if s.location === SkillLocation.Project then 0 else 1, s.name)
      }

      for skill <- sorted do {
        val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
        val locationLabel =
          s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString})".blue + s": $pathLabel".dim
        println(s"  ${skill.name.padTo(25, ' ').bold} $locationLabel")
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
}
