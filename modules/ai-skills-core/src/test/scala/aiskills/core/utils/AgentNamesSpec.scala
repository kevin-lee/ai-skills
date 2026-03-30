package aiskills.core.utils

import aiskills.core.Agent
import hedgehog.*
import hedgehog.runner.*

object AgentNamesSpec extends Properties {

  override def tests: List[Test] = List(
    example("parses single agent", testSingleAgent),
    example("parses comma-separated agents", testCommaSeparated),
    example("trims whitespace", testTrimWhitespace),
    example("deduplicates agents", testDedup),
    example("is case insensitive", testCaseInsensitive),
    example("returns Left for invalid agent", testInvalidAgent),
    example("returns Left for mixed valid and invalid", testMixedValidInvalid),
    example("returns empty list for empty string", testEmptyString),
    example("parses 'all' as all agents", testAllAgents),
    example("parses 'all' case-insensitively", testAllCaseInsensitive),
    example("returns all agents when 'all' appears with other names", testAllWithOtherNames),
  )

  private def testSingleAgent: Result =
    AgentNames.parseAgentNames("claude") ==== Right(List(Agent.Claude))

  private def testCommaSeparated: Result =
    AgentNames.parseAgentNames("claude,cursor") ==== Right(List(Agent.Claude, Agent.Cursor))

  private def testTrimWhitespace: Result =
    AgentNames.parseAgentNames(" claude , cursor ") ==== Right(List(Agent.Claude, Agent.Cursor))

  private def testDedup: Result =
    AgentNames.parseAgentNames("claude,claude") ==== Right(List(Agent.Claude))

  private def testCaseInsensitive: Result =
    AgentNames.parseAgentNames("Claude,CURSOR") ==== Right(List(Agent.Claude, Agent.Cursor))

  private def testInvalidAgent: Result =
    AgentNames.parseAgentNames("invalid") ==== Left("invalid")

  private def testMixedValidInvalid: Result =
    AgentNames.parseAgentNames("claude,bogus") ==== Left("bogus")

  private def testEmptyString: Result =
    AgentNames.parseAgentNames("") ==== Right(List.empty)

  private def testAllAgents: Result =
    AgentNames.parseAgentNames("all") ==== Right(Agent.all)

  private def testAllCaseInsensitive: Result =
    AgentNames.parseAgentNames("ALL") ==== Right(Agent.all)

  private def testAllWithOtherNames: Result =
    AgentNames.parseAgentNames("claude,codex,all") ==== Right(Agent.all)
}
