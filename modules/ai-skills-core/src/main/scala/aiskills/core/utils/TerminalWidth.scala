package aiskills.core.utils

import scala.scalanative.unsafe.*

object TerminalWidth {

  @extern
  private def aiskills_get_terminal_width(): Int = extern

  def getTerminalWidth(): Int = {
    val width = aiskills_get_terminal_width()
    if width > 0 then width else 80
  }
}
