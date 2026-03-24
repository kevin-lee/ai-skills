package aiskills.cli.commands

import aiskills.core.utils.Skills

object Remove {

  /** Remove a specific installed skill. */
  def removeSkill(skillName: String): Unit = {
    val skill = Skills.findSkill(skillName) match {
      case Some(s) => s
      case None =>
        System.err.println(s"Error: Skill '$skillName' not found")
        sys.exit(1)
    }

    os.remove.all(skill.baseDir)

    val scope     = skill.location.toString.toLowerCase
    val agentName = skill.agent.toString
    println(s"\u2705 Removed: $skillName")
    println(s"   From: $scope, $agentName (${skill.source})")
  }
}
