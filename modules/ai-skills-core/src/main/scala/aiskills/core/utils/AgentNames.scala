package aiskills.core.utils

import aiskills.core.Agent
import cats.syntax.all.*

object AgentNames {

  /** Parse comma-separated agent names. Returns Left(firstInvalidName) on failure. */
  def parseAgentNames(input: String): Either[String, List[Agent]] = {
    val names            = input.split(",").toList.map(_.trim).filter(_.nonEmpty).distinct
    val (invalid, valid) = names.partitionMap { name =>
      Agent.fromString(name) match {
        case Some(agent) => agent.asRight
        case None => name.asLeft
      }
    }
    if invalid.nonEmpty then invalid.head.asLeft
    else valid.asRight
  }
}
