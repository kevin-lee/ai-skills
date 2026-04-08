package aiskills.core.utils

import cats.syntax.all.*

object SkillNameValidation {

  private val validNamePattern = "^[a-z0-9]([a-z0-9-]{0,62}[a-z0-9])?$".r

  /** Validate a skill name per the Agent Skills spec.
    * @return
    *   None if valid, Some(errorMessage) if invalid
    */
  def validate(name: String): Option[String] =
    if name.isEmpty then "Name must not be empty".some
    else if name.length > 64 then "Name must be 1-64 characters".some
    else if name.startsWith("-") then "Name must not start with a hyphen".some
    else if name.endsWith("-") then "Name must not end with a hyphen".some
    else if name.contains("--") then "Name must not contain consecutive hyphens (--)".some
    else if validNamePattern.findFirstIn(name).isEmpty
    then "Name must contain only lowercase alphanumeric characters (a-z, 0-9) and hyphens (-)".some
    else none[String]

  /** Validate name and also check that target directory doesn't already exist. */
  def validateWithTarget(name: String, targetDir: os.Path): Option[String] =
    validate(name).orElse {
      val targetPath = targetDir / name
      if os.exists(targetPath) then s"Skill '$name' already exists".some
      else none[String]
    }
}
