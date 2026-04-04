package aiskills.core.utils

import scala.util.matching.Regex

object Yaml {

  private val fieldPattern: String => Regex =
    field => s"""(?m)^${Regex.quote(field)}:\\s*(.+?)$$""".r

  private val blockScalarIndicator: Regex = """^([>|])[-+]?$""".r

  /** Extract a single field value from YAML frontmatter. */
  def extractYamlField(content: String, field: String): String =
    fieldPattern(field).findFirstMatchIn(content) match {
      case Some(m) =>
        val value = m.group(1).trim
        value match {
          case blockScalarIndicator(indicator) =>
            extractBlockScalarValue(content, m.end, isFolded = indicator == ">")
          case _ => value
        }
      case None => ""
    }

  /** Collect indented continuation lines after a block scalar indicator and join them. */
  private def extractBlockScalarValue(content: String, matchEnd: Int, isFolded: Boolean): String = {
    val remaining = content.substring(matchEnd)
    val lines     = remaining.linesIterator.toList

    val indentedLines = lines
      .dropWhile(_.trim.isEmpty)
      .takeWhile(line => line.headOption.exists(_.isWhitespace))

    if indentedLines.isEmpty then ""
    else {
      val indent    = indentedLines.head.indexWhere(!_.isWhitespace)
      val stripped  = indentedLines.map { line =>
        if line.length >= indent then line.substring(indent)
        else line.trim
      }
      val separator = if isFolded then " " else "\n"
      stripped.mkString(separator).trim
    }
  }

  /** Check if content starts with YAML frontmatter delimiter. */
  def hasValidFrontmatter(content: String): Boolean =
    content.trim.startsWith("---")
}
