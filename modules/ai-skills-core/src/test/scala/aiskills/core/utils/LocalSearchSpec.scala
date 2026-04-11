package aiskills.core.utils

import aiskills.core.{Agent, Skill, SkillLocation}
import hedgehog.*
import hedgehog.runner.*

object LocalSearchSpec extends Properties {

  override def tests: List[Test] = List(
    example("fuzzySearch: exact name match scores 100", testExactNameMatch),
    example("fuzzySearch: name prefix match scores 80", testNamePrefixMatch),
    example("fuzzySearch: name contains match scores 60", testNameContainsMatch),
    example("fuzzySearch: description contains match scores 30", testDescriptionContainsMatch),
    example("fuzzySearch: name match takes priority over description match", testNameOverDescription),
    example("fuzzySearch: case insensitive matching", testCaseInsensitive),
    example("fuzzySearch: returns empty for query shorter than 2 chars", testShortQuery),
    example("fuzzySearch: returns empty when no skills match", testNoMatch),
    example("fuzzySearch: returns empty for empty skills list", testEmptySkills),
    example("fuzzySearch: results sorted by score descending", testSortedByScore),
    example("fuzzySearch: filters out zero-score results", testFiltersZeroScore),
    example("fuzzySearch: trims and lowercases query", testTrimsQuery),
    example("fuzzySearch: multiple skills with different scores", testMultipleSkills),
  )

  private def skill(name: String, description: String): Skill =
    Skill(
      name = name,
      description = description,
      location = SkillLocation.Project,
      agent = Agent.Claude,
      path = os.pwd / ".claude" / "skills" / name,
    )

  private def testExactNameMatch: Result = {
    val skills  = List(skill("commit", "Create git commits"))
    val results = LocalSearch.fuzzySearch("commit", skills)
    Result.all(
      List(
        results.length ==== 1,
        results.head.score ==== 100,
      )
    )
  }

  private def testNamePrefixMatch: Result = {
    val skills  = List(skill("commit-work", "Commit and push work"))
    val results = LocalSearch.fuzzySearch("commit", skills)
    Result.all(
      List(
        results.length ==== 1,
        results.head.score ==== 80,
      )
    )
  }

  private def testNameContainsMatch: Result = {
    val skills  = List(skill("git-commit-helper", "Helps with git"))
    val results = LocalSearch.fuzzySearch("commit", skills)
    Result.all(
      List(
        results.length ==== 1,
        results.head.score ==== 60,
      )
    )
  }

  private def testDescriptionContainsMatch: Result = {
    val skills  = List(skill("helper", "Helps you commit code"))
    val results = LocalSearch.fuzzySearch("commit", skills)
    Result.all(
      List(
        results.length ==== 1,
        results.head.score ==== 30,
      )
    )
  }

  private def testNameOverDescription: Result = {
    val skills  = List(skill("commit", "Does commit things"))
    val results = LocalSearch.fuzzySearch("commit", skills)
    // Name exact match (100) should win over description match (30)
    results.head.score ==== 100
  }

  private def testCaseInsensitive: Result = {
    val skills  = List(skill("Git-Commit", "A tool"))
    val results = LocalSearch.fuzzySearch("git-commit", skills)
    Result.all(
      List(
        results.length ==== 1,
        results.head.score ==== 100,
      )
    )
  }

  private def testShortQuery: Result = {
    val skills = List(skill("a-skill", "description"))
    Result.all(
      List(
        LocalSearch.fuzzySearch("", skills) ==== Nil,
        LocalSearch.fuzzySearch("a", skills) ==== Nil,
      )
    )
  }

  private def testNoMatch: Result = {
    val skills  = List(skill("pdf-reader", "Reads PDF files"))
    val results = LocalSearch.fuzzySearch("commit", skills)
    results ==== Nil
  }

  private def testEmptySkills: Result =
    LocalSearch.fuzzySearch("commit", Nil) ==== Nil

  private def testSortedByScore: Result = {
    val skills  = List(
      skill("helper-commit-tool", "A tool"), // contains -> 60
      skill("commit", "Create commits"), // exact -> 100
      skill("commit-work", "Does work"), // prefix -> 80
      skill("something", "Helps you commit changes"), // desc only -> 30
    )
    val results = LocalSearch.fuzzySearch("commit", skills)
    val scores  = results.map(_.score)
    scores ==== List(100, 80, 60, 30)
  }

  private def testFiltersZeroScore: Result = {
    val skills  = List(
      skill("commit", "A tool"),
      skill("unrelated", "Nothing relevant here"),
      skill("pdf-reader", "Reads files"),
    )
    val results = LocalSearch.fuzzySearch("commit", skills)
    results.length ==== 1
  }

  private def testTrimsQuery: Result = {
    val skills  = List(skill("commit", "A tool"))
    val results = LocalSearch.fuzzySearch("  commit  ", skills)
    Result.all(
      List(
        results.length ==== 1,
        results.head.score ==== 100,
      )
    )
  }

  private def testMultipleSkills: Result = {
    val skills  = List(
      skill("commit", "Exact match"),
      skill("commit-msg", "Prefix match"),
      skill("auto-commit", "Contains match"),
    )
    val results = LocalSearch.fuzzySearch("commit", skills)
    Result.all(
      List(
        results.length ==== 3,
        results(0).skill.name ==== "commit",
        results(0).score ==== 100,
        results(1).skill.name ==== "commit-msg",
        results(1).score ==== 80,
        results(2).skill.name ==== "auto-commit",
        results(2).score ==== 60,
      )
    )
  }
}
