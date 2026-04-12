package aiskills.core.utils

import aiskills.core.MarketplaceResult
import io.circe.{Decoder, HCursor}

import scala.util.Try

object MarketplaceSearch {

  private val SkillsShBaseUrl     = "https://skills.sh/api/search"
  private val AgentSkillShBaseUrl = "https://agentskill.sh/api/skills"
  private val CurlTimeout         = "10"
  private val SkillsShLimit       = 20

  // --- skills.sh ---

  final private case class SkillsShSkill(
    name: String,
    skillId: String,
    installs: Long,
    source: String,
  )
  private object SkillsShSkill {
    given Decoder[SkillsShSkill] = (c: HCursor) =>
      for {
        name     <- c.get[String]("name")
        skillId  <- c.get[String]("skillId")
        installs <- c.get[Long]("installs")
        source   <- c.get[String]("source")
      } yield SkillsShSkill(name, skillId, installs, source)
  }

  final private case class SkillsShResponse(skills: List[SkillsShSkill])
  private object SkillsShResponse {
    given Decoder[SkillsShResponse] = (c: HCursor) => c.get[List[SkillsShSkill]]("skills").map(SkillsShResponse(_))
  }

  def searchSkillsSh(query: String): List[MarketplaceResult] = {
    val url = s"$SkillsShBaseUrl?q=${urlEncode(query)}&limit=$SkillsShLimit"
    curlJson(url)
      .flatMap(parseSkillsShJson)
      .getOrElse(Nil)
  }

  /** Parse skills.sh API JSON response into MarketplaceResult list. */
  def parseSkillsShJson(json: String): Option[List[MarketplaceResult]] =
    io.circe.parser.decode[SkillsShResponse](json).toOption.map { response =>
      response.skills.map { skill =>
        MarketplaceResult(
          name = skill.name,
          skillId = skill.skillId,
          source = skill.source,
          description = "",
          installs = skill.installs,
          marketplace = "skills.sh",
        )
      }
    }

  // --- agentskill.sh ---

  final private case class AgentSkillShSkill(
    name: String,
    owner: String,
    description: String,
    installCount: Long,
    githubRepo: String,
  )
  private object AgentSkillShSkill {
    given Decoder[AgentSkillShSkill] = (c: HCursor) =>
      for {
        name         <- c.get[String]("name")
        owner        <- c.get[String]("owner")
        description  <- c.getOrElse[String]("description")("")
        installCount <- c.getOrElse[Long]("installCount")(0L)
        githubRepo   <- c.getOrElse[String]("githubRepo")("")
      } yield AgentSkillShSkill(name, owner, description, installCount, githubRepo)
  }

  final private case class AgentSkillShResponse(data: List[AgentSkillShSkill])
  private object AgentSkillShResponse {
    given Decoder[AgentSkillShResponse] =
      (c: HCursor) => c.get[List[AgentSkillShSkill]]("data").map(AgentSkillShResponse(_))
  }

  def searchAgentSkillSh(query: String): List[MarketplaceResult] = {
    val url = s"$AgentSkillShBaseUrl?q=${urlEncode(query)}"
    curlJson(url)
      .flatMap(parseAgentSkillShJson)
      .getOrElse(Nil)
  }

  /** Parse agentskill.sh API JSON response into MarketplaceResult list. */
  def parseAgentSkillShJson(json: String): Option[List[MarketplaceResult]] =
    io.circe.parser.decode[AgentSkillShResponse](json).toOption.map { response =>
      response.data.map { skill =>
        val source =
          if skill.githubRepo.nonEmpty then s"${skill.owner}/${skill.githubRepo}"
          else skill.owner
        MarketplaceResult(
          name = skill.name,
          skillId = skill.name,
          source = source,
          description = skill.description,
          installs = skill.installCount,
          marketplace = "agentskill.sh",
        )
      }
    }

  // --- Combined search ---

  /** Search both marketplaces and deduplicate by (name, source). */
  def searchAll(query: String): List[MarketplaceResult] = {
    val skillsSh   = searchSkillsSh(query)
    val agentSkill = searchAgentSkillSh(query)
    deduplicateResults(skillsSh ++ agentSkill)
  }

  /** Deduplicate results by (name, source), preferring higher installs and longer descriptions.
    * When the winner has an empty description, it borrows the first non-empty description
    * from the group so useful descriptions survive dedup regardless of which source won.
    */
  def deduplicateResults(results: List[MarketplaceResult]): List[MarketplaceResult] =
    results
      .groupBy(r => (r.name.toLowerCase, r.source.toLowerCase))
      .values
      .map { group =>
        val winner = group.maxBy(r => (r.installs, r.description.length))
        if winner.description.nonEmpty then winner
        else {
          val bestDesc = group.map(_.description).find(_.nonEmpty).getOrElse("")
          winner.copy(description = bestDesc)
        }
      }
      .toList
      .sortBy(-_.installs)

  // --- Utilities ---

  private def curlJson(url: String): Option[String] =
    Try {
      os.proc("curl", "-s", "-f", "--max-time", CurlTimeout, url)
        .call(stderr = os.Pipe)
        .out
        .text()
        .trim
    }.toOption.filter(_.nonEmpty)

  private def urlEncode(s: String): String =
    java.net.URLEncoder.encode(s, "UTF-8")
}
