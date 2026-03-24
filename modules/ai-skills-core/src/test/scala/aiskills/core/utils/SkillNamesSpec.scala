package aiskills.core.utils

import hedgehog.*
import hedgehog.runner.*

object SkillNamesSpec extends Properties {

  override def tests: List[Test] = List(
    example("splits comma-separated names", testSplitComma),
    example("trims whitespace and removes empties", testTrimAndFilter),
    example("supports arrays with comma values", testArrayWithComma),
    example("deduplicates names", testDedup),
    example("returns empty list for empty input", testEmpty),
  )

  private def testSplitComma: Result =
    SkillNames.normalizeSkillNames(List("alpha,beta")) ==== List("alpha", "beta")

  private def testTrimAndFilter: Result =
    SkillNames.normalizeSkillNames(List(" alpha, , beta ,")) ==== List("alpha", "beta")

  private def testArrayWithComma: Result =
    SkillNames.normalizeSkillNames(List("alpha", "beta,gamma")) ==== List("alpha", "beta", "gamma")

  private def testDedup: Result =
    SkillNames.normalizeSkillNames(List("alpha", "alpha", "beta")) ==== List("alpha", "beta")

  private def testEmpty: Result =
    SkillNames.normalizeSkillNames(List.empty) ==== List.empty
}
