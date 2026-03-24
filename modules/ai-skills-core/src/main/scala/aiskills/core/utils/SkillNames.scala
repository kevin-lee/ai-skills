package aiskills.core.utils

object SkillNames {

  /** Normalize skill names: split commas, trim whitespace, deduplicate. */
  def normalizeSkillNames(input: List[String]): List[String] =
    input
      .flatMap(_.split(",").toList)
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct
}
