package aiskills.core

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import io.circe.syntax.*

object AgentSpec extends Properties {

  override def tests: List[Test] = List(
    example("fromString: valid lowercase", testFromStringLowercase),
    example("fromString: valid mixed case", testFromStringMixedCase),
    example("fromString: invalid returns None", testFromStringInvalid),
    example("Universal: projectDirName is .agents", testUniversalProjectDir),
    example("Universal: globalDirName is .agents", testUniversalGlobalDir),
    example("Claude: projectDirName is .claude", testClaudeProjectDir),
    example("Copilot: asymmetric dirs (.github project, .copilot global)", testCopilotAsymmetry),
    example("all: has 7 agents", testAllCount),
    example("allNonUniversal: has 6 agents", testAllNonUniversalCount),
    example("allNonUniversal: excludes Universal", testAllNonUniversalExcludes),
    example("needsAgentsMd: true for Universal and Codex", testNeedsAgentsMd),
    example("needsAgentsMd: false for Claude, Cursor, Gemini, Windsurf, Copilot", testDoesNotNeedAgentsMd),
    example("Encoder/Decoder: round-trip", testEncoderDecoderRoundTrip),
  )

  private def testFromStringLowercase: Result =
    Result.all(
      List(
        Agent.fromString("universal") ==== Agent.Universal.asRight,
        Agent.fromString("claude") ==== Agent.Claude.asRight,
        Agent.fromString("cursor") ==== Agent.Cursor.asRight,
        Agent.fromString("codex") ==== Agent.Codex.asRight,
        Agent.fromString("gemini") ==== Agent.Gemini.asRight,
        Agent.fromString("windsurf") ==== Agent.Windsurf.asRight,
        Agent.fromString("copilot") ==== Agent.Copilot.asRight,
      )
    )

  private def testFromStringMixedCase: Result =
    Result.all(
      List(
        Agent.fromString("Claude") ==== Agent.Claude.asRight,
        Agent.fromString("CURSOR") ==== Agent.Cursor.asRight,
        Agent.fromString("Universal") ==== Agent.Universal.asRight,
      )
    )

  private def testFromStringInvalid: Result = {
    val input1 = "invalid"
    val input2 = ""
    val input3 = "vscode"
    Result.all(
      List(
        Agent.fromString(
          input1
        ) ==== s"Invalid Agent: $input1. Valid agents: ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
          .asLeft[Agent],
        Agent.fromString(
          input2
        ) ==== s"Invalid Agent: $input2. Valid agents: ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
          .asLeft[Agent],
        Agent.fromString(
          input3
        ) ==== s"Invalid Agent: $input3. Valid agents: ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
          .asLeft[Agent],
      )
    )
  }

  private def testUniversalProjectDir: Result =
    Agent.Universal.projectDirName ==== ".agents"

  private def testUniversalGlobalDir: Result =
    Agent.Universal.globalDirName ==== ".agents"

  private def testClaudeProjectDir: Result =
    Agent.Claude.projectDirName ==== ".claude"

  private def testCopilotAsymmetry: Result =
    Result.all(
      List(
        Agent.Copilot.projectDirName ==== ".github",
        Agent.Copilot.globalDirName ==== ".copilot",
      )
    )

  private def testAllCount: Result =
    Agent.all.length ==== 7

  private def testAllNonUniversalCount: Result =
    Agent.allNonUniversal.length ==== 6

  private def testAllNonUniversalExcludes: Result =
    Result.assert(!Agent.allNonUniversal.contains(Agent.Universal))

  private def testNeedsAgentsMd: Result =
    Result.all(
      List(
        Result.assert(Agent.needsAgentsMd(Agent.Universal)),
        Result.assert(Agent.needsAgentsMd(Agent.Codex)),
      )
    )

  private def testDoesNotNeedAgentsMd: Result =
    Result.all(
      List(
        Result.assert(!Agent.needsAgentsMd(Agent.Claude)),
        Result.assert(!Agent.needsAgentsMd(Agent.Cursor)),
        Result.assert(!Agent.needsAgentsMd(Agent.Gemini)),
        Result.assert(!Agent.needsAgentsMd(Agent.Windsurf)),
        Result.assert(!Agent.needsAgentsMd(Agent.Copilot)),
      )
    )

  private def testEncoderDecoderRoundTrip: Result =
    Result.all(
      Agent.all.map { agent =>
        val json    = agent.asJson
        val decoded = json.as[Agent]
        decoded ==== Right(agent)
      }
    )
}
