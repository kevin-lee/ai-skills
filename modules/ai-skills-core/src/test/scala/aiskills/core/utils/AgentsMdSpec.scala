package aiskills.core.utils

import aiskills.core.{Agent, Skill, SkillLocation}
import hedgehog.*
import hedgehog.runner.*

object AgentsMdSpec extends Properties {

  override def tests: List[Test] = List(
    example("generateSkillsXml: generates valid XML for skills", testGenerateXml),
    example("generateSkillsXml: includes usage instructions", testUsageInstructions),
    example("generateSkillsXml: generates empty section for empty array", testEmptySkills),
    example("parseCurrentSkills: parses skill names from content", testParseSkills),
    example("parseCurrentSkills: returns empty for no skills", testParseEmpty),
    example("parseCurrentSkills: handles malformed XML gracefully", testParseMalformed),
    example("replaceSkillsSection: replaces skills-system section", testReplaceXml),
    example("replaceSkillsSection: replaces HTML comment markers", testReplaceHtml),
    example("replaceSkillsSection: appends if no markers", testAppend),
    example("removeSkillsSection: removes skills-system section", testRemoveXml),
    example("removeSkillsSection: handles no section", testRemoveNone),
    example("generateSkillsXml: escapes special characters in skill data", testXmlEscaping),
    example("replaceSkillsSection: replaces old skills_system format", testReplaceOldFormat),
    example("removeSkillsSection: removes old skills_system format", testRemoveOldFormat),
    example("parseCurrentSkills: parses skills from kebab-case tags", testParseKebabCase),
    example("generateSkillsXml: includes agent element", testAgentElement),
  )

  private val sampleSkills = List(
    Skill("pdf", "PDF manipulation", SkillLocation.Project, Agent.Claude, os.pwd / "path" / "to" / "pdf"),
    Skill("xlsx", "Spreadsheet editing", SkillLocation.Global, Agent.Universal, os.home / "path" / "to" / "xlsx"),
  )

  private def testGenerateXml: Result = {
    val xml = AgentsMd.generateSkillsXml(sampleSkills)
    Result.all(
      List(
        Result.assert(xml.contains("""<skills-system priority="1">""")),
        Result.assert(xml.contains("<name>pdf</name>")),
        Result.assert(xml.contains("<description>PDF manipulation</description>")),
        Result.assert(xml.contains("<location>project</location>")),
        Result.assert(xml.contains("<name>xlsx</name>")),
        Result.assert(xml.contains("<description>Spreadsheet editing</description>")),
        Result.assert(xml.contains("<location>global</location>")),
        Result.assert(xml.contains("</skills-system>")),
      )
    )
  }

  private def testUsageInstructions: Result = {
    val xml = AgentsMd.generateSkillsXml(
      List(
        Skill("test", "Test skill", SkillLocation.Project, Agent.Claude, os.pwd / "path"),
      )
    )
    Result.all(
      List(
        Result.assert(xml.contains("<usage>")),
        Result.assert(xml.contains("aiskills read")),
        Result.assert(xml.contains("</usage>")),
      )
    )
  }

  private def testEmptySkills: Result = {
    val xml = AgentsMd.generateSkillsXml(Nil)
    Result.all(
      List(
        Result.assert(xml.contains("<available-skills>")),
        Result.assert(xml.contains("</available-skills>")),
      )
    )
  }

  private def testParseSkills: Result = {
    val content =
      """# AGENTS.md
        |
        |<skills-system>
        |<available-skills>
        |<skill>
        |<name>pdf</name>
        |<description>PDF tools</description>
        |</skill>
        |<skill>
        |<name>xlsx</name>
        |<description>Excel tools</description>
        |</skill>
        |</available-skills>
        |</skills-system>""".stripMargin

    val skills = AgentsMd.parseCurrentSkills(content)
    Result.all(
      List(
        Result.assert(skills.contains("pdf")),
        Result.assert(skills.contains("xlsx")),
        skills.length ==== 2,
      )
    )
  }

  private def testParseEmpty: Result =
    AgentsMd.parseCurrentSkills("# AGENTS.md\n\nNo skills here.").length ==== 0

  private def testParseMalformed: Result = {
    val skills = AgentsMd.parseCurrentSkills("<skill><name>broken")
    Result.assert(skills.isEmpty)
  }

  private def testReplaceXml: Result = {
    val content =
      """# AGENTS.md
        |
        |<skills-system priority="1">
        |OLD CONTENT
        |</skills-system>
        |
        |Other content""".stripMargin

    val newSection = """<skills-system priority="1">NEW CONTENT</skills-system>"""
    val result     = AgentsMd.replaceSkillsSection(content, newSection)
    Result.all(
      List(
        Result.assert(result.contains("NEW CONTENT")),
        Result.assert(!result.contains("OLD CONTENT")),
        Result.assert(result.contains("Other content")),
      )
    )
  }

  private def testReplaceHtml: Result = {
    val content =
      """# AGENTS.md
        |
        |<!-- SKILLS_TABLE_START -->
        |OLD SKILLS
        |<!-- SKILLS_TABLE_END -->
        |
        |Footer""".stripMargin

    val newSection = "<skills-system>NEW SKILLS</skills-system>"
    val result     = AgentsMd.replaceSkillsSection(content, newSection)
    Result.all(
      List(
        Result.assert(result.contains("NEW SKILLS")),
        Result.assert(!result.contains("OLD SKILLS")),
      )
    )
  }

  private def testAppend: Result = {
    val content    = "# AGENTS.md\n\nSome content."
    val newSection = "<skills-system>SKILLS</skills-system>"
    val result     = AgentsMd.replaceSkillsSection(content, newSection)
    Result.all(
      List(
        Result.assert(result.contains("Some content.")),
        Result.assert(result.contains("<skills-system>SKILLS</skills-system>")),
      )
    )
  }

  private def testRemoveXml: Result = {
    val content =
      """# AGENTS.md
        |
        |<skills-system priority="1">
        |Skills content
        |</skills-system>
        |
        |Footer""".stripMargin

    val result = AgentsMd.removeSkillsSection(content)
    Result.all(
      List(
        Result.assert(!result.contains("Skills content")),
        Result.assert(result.contains("Footer")),
      )
    )
  }

  private def testRemoveNone: Result = {
    val content = "# AGENTS.md\n\nNo skills."
    AgentsMd.removeSkillsSection(content) ==== content
  }

  private def testXmlEscaping: Result = {
    val skills = List(
      Skill(
        "test & <demo>",
        """Handles "quotes" & <special> chars""",
        SkillLocation.Project,
        Agent.Claude,
        os.pwd / "path",
      ),
    )
    val xml    = AgentsMd.generateSkillsXml(skills)
    Result.all(
      List(
        Result.assert(xml.contains("&amp;")),
        Result.assert(xml.contains("&lt;")),
        Result.assert(xml.contains("&gt;")),
        Result.assert(!xml.contains("<demo>")),
        Result.assert(!xml.contains("<special>")),
        Result.assert(xml.contains("test &amp; &lt;demo&gt;")),
        Result.assert(xml.contains("&quot;")),
        Result.assert(xml.contains("Handles &quot;quotes&quot; &amp; &lt;special&gt; chars")),
      )
    )
  }

  private def testReplaceOldFormat: Result = {
    val content =
      """# AGENTS.md
        |
        |<skills_system priority="1">
        |OLD CONTENT
        |</skills_system>
        |
        |Other content""".stripMargin

    val newSection = """<skills-system priority="1">NEW CONTENT</skills-system>"""
    val result     = AgentsMd.replaceSkillsSection(content, newSection)
    Result.all(
      List(
        Result.assert(result.contains("NEW CONTENT")),
        Result.assert(!result.contains("OLD CONTENT")),
        Result.assert(result.contains("Other content")),
        Result.assert(!result.contains("skills_system")),
      )
    )
  }

  private def testRemoveOldFormat: Result = {
    val content =
      """# AGENTS.md
        |
        |<skills_system priority="1">
        |Skills content
        |</skills_system>
        |
        |Footer""".stripMargin

    val result = AgentsMd.removeSkillsSection(content)
    Result.all(
      List(
        Result.assert(!result.contains("Skills content")),
        Result.assert(result.contains("Footer")),
        Result.assert(!result.contains("skills_system")),
      )
    )
  }

  private def testParseKebabCase: Result = {
    val content =
      """# AGENTS.md
        |
        |<skills-system>
        |<available-skills>
        |<skill>
        |<name>pdf</name>
        |<description>PDF tools</description>
        |</skill>
        |</available-skills>
        |</skills-system>""".stripMargin

    val skills = AgentsMd.parseCurrentSkills(content)
    Result.all(
      List(
        Result.assert(skills.contains("pdf")),
        skills.length ==== 1,
      )
    )
  }

  private def testAgentElement: Result = {
    val xml = AgentsMd.generateSkillsXml(sampleSkills)
    Result.all(
      List(
        Result.assert(xml.contains("<agent>claude</agent>")),
        Result.assert(xml.contains("<agent>universal</agent>")),
      )
    )
  }
}
