package aiskills.cli

import cue4s.Prompt

object CliDefaults {
  val MultiChoiceWindowSize: Int = 20

  val multiChoiceModify: Prompt.MultipleChoice => Prompt.MultipleChoice =
    _.withWindowSize(MultiChoiceWindowSize)
}
