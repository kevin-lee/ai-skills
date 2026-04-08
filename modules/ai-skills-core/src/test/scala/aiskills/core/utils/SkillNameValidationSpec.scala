package aiskills.core.utils

import hedgehog.*
import hedgehog.runner.*

object SkillNameValidationSpec extends Properties {

  override def tests: List[Test] = List(
    example("validate: accepts valid names", testValidNames),
    example("validate: accepts single character", testSingleChar),
    example("validate: accepts two characters", testTwoChars),
    example("validate: accepts max length (64 chars)", testMaxLength),
    example("validate: rejects empty name", testEmpty),
    example("validate: rejects names over 64 chars", testTooLong),
    example("validate: rejects leading hyphen", testLeadingHyphen),
    example("validate: rejects trailing hyphen", testTrailingHyphen),
    example("validate: rejects consecutive hyphens", testConsecutiveHyphens),
    example("validate: rejects uppercase letters", testUppercase),
    example("validate: rejects special characters", testSpecialChars),
    example("validate: rejects spaces", testSpaces),
    example("validateWithTarget: rejects existing directory", testExistingDir),
    example("validateWithTarget: accepts non-existing directory", testNonExistingDir),
  )

  private def testValidNames: Result =
    Result.all(
      List(
        Result.assert(SkillNameValidation.validate("my-skill").isEmpty),
        Result.assert(SkillNameValidation.validate("skill-name-v2").isEmpty),
        Result.assert(SkillNameValidation.validate("abc123").isEmpty),
        Result.assert(SkillNameValidation.validate("a1b2c3").isEmpty),
      )
    )

  private def testSingleChar: Result =
    Result.all(
      List(
        Result.assert(SkillNameValidation.validate("a").isEmpty),
        Result.assert(SkillNameValidation.validate("0").isEmpty),
      )
    )

  private def testTwoChars: Result =
    Result.all(
      List(
        Result.assert(SkillNameValidation.validate("ab").isEmpty),
        Result.assert(SkillNameValidation.validate("a1").isEmpty),
      )
    )

  private def testMaxLength: Result = {
    val name = "a" * 64
    Result.assert(SkillNameValidation.validate(name).isEmpty)
  }

  private def testEmpty: Result =
    Result.assert(SkillNameValidation.validate("").isDefined)

  private def testTooLong: Result = {
    val name = "a" * 65
    Result.assert(SkillNameValidation.validate(name).isDefined)
  }

  private def testLeadingHyphen: Result =
    Result.assert(SkillNameValidation.validate("-my-skill").isDefined)

  private def testTrailingHyphen: Result =
    Result.assert(SkillNameValidation.validate("my-skill-").isDefined)

  private def testConsecutiveHyphens: Result =
    Result.assert(SkillNameValidation.validate("my--skill").isDefined)

  private def testUppercase: Result =
    Result.all(
      List(
        Result.assert(SkillNameValidation.validate("My-Skill").isDefined),
        Result.assert(SkillNameValidation.validate("SKILL").isDefined),
        Result.assert(SkillNameValidation.validate("mySkill").isDefined),
      )
    )

  private def testSpecialChars: Result =
    Result.all(
      List(
        Result.assert(SkillNameValidation.validate("my_skill").isDefined),
        Result.assert(SkillNameValidation.validate("my.skill").isDefined),
        Result.assert(SkillNameValidation.validate("my/skill").isDefined),
        Result.assert(SkillNameValidation.validate("my@skill").isDefined),
      )
    )

  private def testSpaces: Result =
    Result.all(
      List(
        Result.assert(SkillNameValidation.validate("my skill").isDefined),
        Result.assert(SkillNameValidation.validate(" skill").isDefined),
      )
    )

  private def testExistingDir: Result = {
    val tmpDir = os.temp.dir()
    try {
      val existingSkill = tmpDir / "existing-skill"
      os.makeDir(existingSkill)
      Result.assert(SkillNameValidation.validateWithTarget("existing-skill", tmpDir).isDefined)
    } finally os.remove.all(tmpDir)
  }

  private def testNonExistingDir: Result = {
    val tmpDir = os.temp.dir()
    try Result.assert(SkillNameValidation.validateWithTarget("new-skill", tmpDir).isEmpty)
    finally os.remove.all(tmpDir)
  }
}
