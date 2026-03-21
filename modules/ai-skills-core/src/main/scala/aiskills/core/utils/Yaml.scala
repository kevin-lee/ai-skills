package aiskills.core.utils

import scala.util.matching.Regex

object Yaml:

  private val fieldPattern: String => Regex =
    field => s"""(?m)^${Regex.quote(field)}:\\s*(.+?)$$""".r

  /** Extract a single field value from YAML frontmatter. */
  def extractYamlField(content: String, field: String): String =
    fieldPattern(field).findFirstMatchIn(content) match
      case Some(m) => m.group(1).trim
      case None    => ""

  /** Check if content starts with YAML frontmatter delimiter. */
  def hasValidFrontmatter(content: String): Boolean =
    content.trim.startsWith("---")
