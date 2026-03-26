package aiskills.core.utils

import aiskills.core.{Agent, Skill, SkillLocation}

import scala.util.matching.Regex
import scala.xml.{Elem, PrettyPrinter}

object AgentsMd {

  /** Parse skill names currently in AGENTS.md content. */
  def parseCurrentSkills(content: String): List[String] = {
    val skillRegex = """<skill>[\s\S]*?<name>([^<]+)</name>[\s\S]*?</skill>""".r
    skillRegex.findAllMatchIn(content).map(_.group(1).trim).toList
  }

  /** Generate skills XML section for AGENTS.md. */
  def generateSkillsXml(skills: List[Skill]): String = {
    val printer = new PrettyPrinter(120, 0)

    val skillTags = skills
      .map { s =>
        // scalafix:off DisableSyntax.noXml
        val skillElem: Elem =
          <skill>
            <name>{s.name}</name>
            <description>{s.description}</description>
            <location>{s.location.toString.toLowerCase}</location>
            <agent>{s.agent.toString.toLowerCase}</agent>
          </skill>
        // scalafix:on
        printer.format(skillElem)
      }
      .mkString("\n\n")

    s"""<skills-system priority="1">
       |
       |## Available Skills
       |
       |<!-- SKILLS_TABLE_START -->
       |<usage>
       |When users ask you to perform tasks, check if any of the available skills below can help complete the task more effectively. Skills provide specialized capabilities and domain knowledge.
       |
       |How to use skills:
       |- Invoke: `aiskills read <skill-name>` (run in your shell)
       |  - For multiple: `aiskills read skill-one,skill-two`
       |- The skill content will load with detailed instructions on how to complete the task
       |- Base directory provided in output for resolving bundled resources (references/, scripts/, assets/)
       |
       |Usage notes:
       |- Only use skills listed in <available-skills> below
       |- Do not invoke a skill that is already loaded in your context
       |- Each skill invocation is stateless
       |</usage>
       |
       |<available-skills>
       |
       |$skillTags
       |
       |</available-skills>
       |<!-- SKILLS_TABLE_END -->
       |
       |</skills-system>""".stripMargin
  }

  /** Replace or add skills section in AGENTS.md content. */
  def replaceSkillsSection(content: String, newSection: String): String = {
    val xmlMarkerStart    = "<skills-system"
    val xmlMarkerStartOld = "<skills_system"
    val xmlRegex          = """<skills[_-]system[^>]*>[\s\S]*?</skills[_-]system>""".r

    if content.contains(xmlMarkerStart) || content
      .contains(xmlMarkerStartOld) then xmlRegex.replaceFirstIn(content, Regex.quoteReplacement(newSection))
    else {
      val htmlStart = "<!-- SKILLS_TABLE_START -->"
      val htmlEnd   = "<!-- SKILLS_TABLE_END -->"

      if content.contains(htmlStart) then {
        val innerContent = newSection
          .replaceAll("""<skills[_-]system[^>]*>""", "")
          .replace("</skills-system>", "")
          .replace("</skills_system>", "")
        val htmlRegex    = s"""(?s)${Regex.quote(htmlStart)}[\\s\\S]*?${Regex.quote(htmlEnd)}""".r
        htmlRegex.replaceFirstIn(
          content,
          Regex.quoteReplacement(s"$htmlStart\n$innerContent\n$htmlEnd"),
        )
      } else
        content.stripTrailing + "\n\n" + newSection + "\n"
    }
  }

  /** Remove skills section from AGENTS.md content. */
  def removeSkillsSection(content: String): String = {
    val xmlMarkerStart    = "<skills-system"
    val xmlMarkerStartOld = "<skills_system"
    val xmlRegex          = """<skills[_-]system[^>]*>[\s\S]*?</skills[_-]system>""".r

    if content.contains(xmlMarkerStart) || content
      .contains(xmlMarkerStartOld) then xmlRegex.replaceFirstIn(content, "<!-- Skills section removed -->")
    else {
      val htmlStart = "<!-- SKILLS_TABLE_START -->"
      val htmlEnd   = "<!-- SKILLS_TABLE_END -->"

      if content.contains(htmlStart) then {
        val htmlRegex = s"""(?s)${Regex.quote(htmlStart)}[\\s\\S]*?${Regex.quote(htmlEnd)}""".r
        htmlRegex.replaceFirstIn(
          content,
          Regex.quoteReplacement(s"$htmlStart\n<!-- Skills section removed -->\n$htmlEnd"),
        )
      } else
        content
    }
  }

  /** Update AGENTS.md for agents that need it (e.g., Codex, Universal). */
  def updateAgentsMdForAgent(agent: Agent, location: SkillLocation): Unit = {
    if Agent.needsAgentsMd(agent) then {
      val outputPath = location match {
        case SkillLocation.Global => os.home / "AGENTS.md"
        case SkillLocation.Project => os.pwd / "AGENTS.md"
      }
      val skills     = Skills.findAllSkills()
      if skills.nonEmpty then {
        val xml = generateSkillsXml(skills)
        if os.exists(outputPath) then {
          val content = os.read(outputPath)
          val updated = replaceSkillsSection(content, xml)
          os.write.over(outputPath, updated)
        } else
          os.write(outputPath, s"# AGENTS\n\n$xml\n")
      } else ()
    } else ()
  }
}
