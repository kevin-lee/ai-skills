package aiskills.core.utils

import hedgehog.*
import hedgehog.runner.*

object YamlSpec extends Properties:

  override def tests: List[Test] = List(
    example("extractYamlField: should extract field from YAML frontmatter", testExtractField),
    example("extractYamlField: should return empty string if field not found", testMissingField),
    example("extractYamlField: should handle multiline descriptions", testMultilineDesc),
    example("extractYamlField: should use non-greedy matching (security)", testNonGreedy),
    example("extractYamlField: should not be vulnerable to ReDoS patterns", testReDoS),
    example("extractYamlField: should handle special characters in values", testSpecialChars),
    example("extractYamlField: should handle colons in values", testColonsInValues),
    example("hasValidFrontmatter: should return true for valid frontmatter", testValidFrontmatter),
    example("hasValidFrontmatter: should return false for missing frontmatter", testMissingFrontmatter),
    example("hasValidFrontmatter: should return false for empty content", testEmptyContent),
  )

  private def testExtractField: Result =
    val content =
      """---
        |name: test-skill
        |description: Test description
        |---
        |
        |Content""".stripMargin

    Result.all(List(
      Yaml.extractYamlField(content, "name") ==== "test-skill",
      Yaml.extractYamlField(content, "description") ==== "Test description",
    ))

  private def testMissingField: Result =
    val content =
      """---
        |name: test-skill
        |---""".stripMargin

    Yaml.extractYamlField(content, "missing") ==== ""

  private def testMultilineDesc: Result =
    val content =
      """---
        |name: test
        |description: First line
        |---""".stripMargin

    Yaml.extractYamlField(content, "description") ==== "First line"

  private def testNonGreedy: Result =
    val content =
      """---
        |name: skill-name
        |description: Short desc
        |other: value
        |---""".stripMargin

    val name = Yaml.extractYamlField(content, "name")
    Result.all(List(
      name ==== "skill-name",
      Result.assert(!name.contains("description")),
    ))

  private def testReDoS: Result =
    val content = s"---\nname: ${"a" * 1000}\n---"
    val start = System.currentTimeMillis()
    val _ = Yaml.extractYamlField(content, "name")
//    println(s"name=$name")
    val elapsed = System.currentTimeMillis() - start
    Result.assert(elapsed < 100)

  private def testSpecialChars: Result =
    val content =
      """---
        |name: skill-with-special_chars.v2
        |description: Contains "quotes" and 'apostrophes'
        |---""".stripMargin

    Yaml.extractYamlField(content, "name") ==== "skill-with-special_chars.v2"

  private def testColonsInValues: Result =
    val content =
      """---
        |name: my-skill
        |description: URL: https://example.com
        |---""".stripMargin

    val desc = Yaml.extractYamlField(content, "description")
    Result.assert(desc.contains("URL:"))

  private def testValidFrontmatter: Result =
    val content =
      """---
        |name: test
        |---
        |
        |Content""".stripMargin

    Result.assert(Yaml.hasValidFrontmatter(content))

  private def testMissingFrontmatter: Result =
    Result.assert(!Yaml.hasValidFrontmatter("No frontmatter here"))

  private def testEmptyContent: Result =
    Result.assert(!Yaml.hasValidFrontmatter(""))
