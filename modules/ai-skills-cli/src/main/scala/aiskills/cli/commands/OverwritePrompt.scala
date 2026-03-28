package aiskills.cli.commands

import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*

object OverwritePrompt {

  enum OverwriteChoice {
    case Yes
    case No
    case YesToAll
    case NoToAll
  }

  enum BulkDecision {
    case Undecided
    case OverwriteAll
    case SkipAll
  }

  def askOverwriteChoice(
    skillName: String,
    promptMessage: String,
  ): Either[Int, OverwriteChoice] = {
    val options = List(
      "Yes          — Overwrite this skill",
      "No           — Skip this skill",
      "Yes to all   — Overwrite all remaining conflicts",
      "No to all    — Skip all remaining conflicts",
    )
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      println(
        s"\u26a0 All existing files and folders in '$skillName' will be removed if you choose to overwrite.".yellow
      )
      prompts.singleChoice(promptMessage.yellow, options) match {
        case Completion.Finished(selected) =>
          if selected.startsWith("Yes to all") then OverwriteChoice.YesToAll.asRight[Int]
          else if selected.startsWith("No to all") then OverwriteChoice.NoToAll.asRight[Int]
          else if selected.startsWith("Yes") then OverwriteChoice.Yes.asRight[Int]
          else OverwriteChoice.No.asRight[Int]
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft[OverwriteChoice]
        case Completion.Fail(CompletionError.Error(_)) =>
          OverwriteChoice.No.asRight[Int]
      }
    }
  }
}
