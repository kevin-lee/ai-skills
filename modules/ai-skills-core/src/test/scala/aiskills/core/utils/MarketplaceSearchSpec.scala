package aiskills.core.utils

import aiskills.core.MarketplaceResult
import hedgehog.*
import hedgehog.runner.*

object MarketplaceSearchSpec extends Properties {

  override def tests: List[Test] = List(
    // skills.sh JSON parsing
    example("parseSkillsShJson: parses valid response", testParseSkillsShValid),
    example("parseSkillsShJson: returns None for invalid JSON", testParseSkillsShInvalid),
    example("parseSkillsShJson: returns None for empty string", testParseSkillsShEmpty),
    example("parseSkillsShJson: handles empty skills array", testParseSkillsShEmptyArray),
    // agentskill.sh JSON parsing
    example("parseAgentSkillShJson: parses valid response", testParseAgentSkillShValid),
    example("parseAgentSkillShJson: constructs source from owner and githubRepo", testParseAgentSkillShSource),
    example("parseAgentSkillShJson: uses owner when githubRepo is empty", testParseAgentSkillShOwnerOnly),
    example("parseAgentSkillShJson: returns None for invalid JSON", testParseAgentSkillShInvalid),
    example("parseAgentSkillShJson: handles missing optional fields", testParseAgentSkillShOptionalFields),
    // Deduplication
    example("deduplicateResults: removes duplicates by name and source", testDeduplicateRemovesDuplicates),
    example("deduplicateResults: prefers higher installs", testDeduplicatePrefersHigherInstalls),
    example("deduplicateResults: prefers longer description on tie", testDeduplicatePrefersLongerDesc),
    example("deduplicateResults: case-insensitive deduplication", testDeduplicateCaseInsensitive),
    example("deduplicateResults: sorts by installs descending", testDeduplicateSortOrder),
    example("deduplicateResults: returns empty for empty input", testDeduplicateEmpty),
    example("deduplicateResults: keeps distinct skills", testDeduplicateKeepsDistinct),
  )

  // --- skills.sh ---

  private def testParseSkillsShValid: Result = {
    val json    =
      """{"query":"commit","searchType":"fuzzy","skills":[{"id":"owner/repo/skill","skillId":"skill","name":"git-commit","installs":1000,"source":"owner/repo"}],"count":1}"""
    val results = MarketplaceSearch.parseSkillsShJson(json)
    Result.all(
      List(
        Result.assert(results.isDefined),
        results.get.length ==== 1,
        results.get.head.name ==== "git-commit",
        results.get.head.source ==== "owner/repo",
        results.get.head.installs ==== 1000L,
        results.get.head.marketplace ==== "skills.sh",
        results.get.head.description ==== "",
      )
    )
  }

  private def testParseSkillsShInvalid: Result =
    MarketplaceSearch.parseSkillsShJson("not json") ==== None

  private def testParseSkillsShEmpty: Result =
    MarketplaceSearch.parseSkillsShJson("") ==== None

  private def testParseSkillsShEmptyArray: Result = {
    val json    = """{"skills":[],"count":0}"""
    val results = MarketplaceSearch.parseSkillsShJson(json)
    Result.all(
      List(
        Result.assert(results.isDefined),
        results.get ==== Nil,
      )
    )
  }

  // --- agentskill.sh ---

  private def testParseAgentSkillShValid: Result = {
    val json    =
      """{"data":[{"name":"commit","owner":"test-user","description":"A commit skill","installCount":500,"githubRepo":"my-skills"}],"total":1}"""
    val results = MarketplaceSearch.parseAgentSkillShJson(json)
    Result.all(
      List(
        Result.assert(results.isDefined),
        results.get.length ==== 1,
        results.get.head.name ==== "commit",
        results.get.head.installs ==== 500L,
        results.get.head.marketplace ==== "agentskill.sh",
        results.get.head.description ==== "A commit skill",
      )
    )
  }

  private def testParseAgentSkillShSource: Result = {
    val json    =
      """{"data":[{"name":"skill","owner":"alice","description":"","installCount":0,"githubRepo":"agent-skills"}]}"""
    val results = MarketplaceSearch.parseAgentSkillShJson(json)
    results.get.head.source ==== "alice/agent-skills"
  }

  private def testParseAgentSkillShOwnerOnly: Result = {
    val json    =
      """{"data":[{"name":"skill","owner":"alice","description":"","installCount":0,"githubRepo":""}]}"""
    val results = MarketplaceSearch.parseAgentSkillShJson(json)
    results.get.head.source ==== "alice"
  }

  private def testParseAgentSkillShInvalid: Result =
    MarketplaceSearch.parseAgentSkillShJson("{invalid}") ==== None

  private def testParseAgentSkillShOptionalFields: Result = {
    // description, installCount, githubRepo all have defaults
    val json    =
      """{"data":[{"name":"skill","owner":"bob"}]}"""
    val results = MarketplaceSearch.parseAgentSkillShJson(json)
    Result.all(
      List(
        Result.assert(results.isDefined),
        results.get.head.description ==== "",
        results.get.head.installs ==== 0L,
        results.get.head.source ==== "bob",
      )
    )
  }

  // --- Deduplication ---

  private def testDeduplicateRemovesDuplicates: Result = {
    val results = List(
      MarketplaceResult("commit", "owner/repo", "desc1", 100, "skills.sh"),
      MarketplaceResult("commit", "owner/repo", "desc2", 200, "agentskill.sh"),
    )
    MarketplaceSearch.deduplicateResults(results).length ==== 1
  }

  private def testDeduplicatePrefersHigherInstalls: Result = {
    val results = List(
      MarketplaceResult("commit", "owner/repo", "", 100, "skills.sh"),
      MarketplaceResult("commit", "owner/repo", "", 500, "agentskill.sh"),
    )
    val deduped = MarketplaceSearch.deduplicateResults(results)
    deduped.head.installs ==== 500L
  }

  private def testDeduplicatePrefersLongerDesc: Result = {
    val results = List(
      MarketplaceResult("commit", "owner/repo", "", 100, "skills.sh"),
      MarketplaceResult("commit", "owner/repo", "A useful skill", 100, "agentskill.sh"),
    )
    val deduped = MarketplaceSearch.deduplicateResults(results)
    deduped.head.description ==== "A useful skill"
  }

  private def testDeduplicateCaseInsensitive: Result = {
    val results = List(
      MarketplaceResult("Commit", "Owner/Repo", "", 100, "skills.sh"),
      MarketplaceResult("commit", "owner/repo", "", 200, "agentskill.sh"),
    )
    MarketplaceSearch.deduplicateResults(results).length ==== 1
  }

  private def testDeduplicateSortOrder: Result = {
    val results = List(
      MarketplaceResult("alpha", "a/repo", "", 50, "skills.sh"),
      MarketplaceResult("beta", "b/repo", "", 200, "skills.sh"),
      MarketplaceResult("gamma", "c/repo", "", 100, "skills.sh"),
    )
    val deduped = MarketplaceSearch.deduplicateResults(results)
    val names   = deduped.map(_.name)
    names ==== List("beta", "gamma", "alpha")
  }

  private def testDeduplicateEmpty: Result =
    MarketplaceSearch.deduplicateResults(Nil) ==== Nil

  private def testDeduplicateKeepsDistinct: Result = {
    val results = List(
      MarketplaceResult("commit", "owner/repo-a", "", 100, "skills.sh"),
      MarketplaceResult("commit", "owner/repo-b", "", 200, "skills.sh"),
      MarketplaceResult("pdf", "owner/repo-a", "", 50, "skills.sh"),
    )
    MarketplaceSearch.deduplicateResults(results).length ==== 3
  }
}
