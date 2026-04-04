package aiskills.core.utils

import aiskills.core.Agent
import cats.syntax.all.*

object AgentNames {

  /** Parse comma-separated agent names. Returns Left(firstInvalidName) on failure.
    * The special value "all" (case-insensitive) resolves to all agents.
    * If "all" appears anywhere in the list (e.g. "claude,all"), it is treated as "all".
    */
  def parseAgentNames(input: String): Either[String, List[Agent]] = {
    val names = input.split(",").toList.map(_.trim).filter(_.nonEmpty).distinct
    if names.exists(_.equalsIgnoreCase("all")) then Agent.all.asRight
    else {
      val (invalid, valid) = names.partitionMap { name =>
        Agent.fromString(name) match {
          case Right(agent) => agent.asRight
          case Left(_) => name.asLeft
        }
      }
      invalid.headOption.toLeft(valid)
    }
  }
}
