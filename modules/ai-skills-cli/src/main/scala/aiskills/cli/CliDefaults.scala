package aiskills.cli

import cue4s.{Prompt, PromptError}

object CliDefaults {
  val MultiChoiceWindowSize: Int = 20

  val multiChoiceModify: Prompt.MultipleChoice => Prompt.MultipleChoice =
    _.withWindowSize(MultiChoiceWindowSize)

  def mandatoryMultiChoiceNoneSelected(label: String, options: List[String]): Prompt[List[String]] =
    multiChoiceModify(Prompt.MultipleChoice.withNoneSelected(label, options))
      .mapValidated(ls =>
        Either.cond(ls.nonEmpty, ls, PromptError("Please select at least one, or press Ctrl+C to cancel."))
      )

  def mandatoryMultiChoiceAllSelected(label: String, options: List[String]): Prompt[List[String]] =
    multiChoiceModify(Prompt.MultipleChoice.withAllSelected(label, options))
      .mapValidated(ls =>
        Either.cond(ls.nonEmpty, ls, PromptError("Please select at least one, or press Ctrl+C to cancel."))
      )
}
