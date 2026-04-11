package aiskills.core

import cats.*
import cats.derived.*

enum SearchLocation derives Eq, Show {
  case Marketplace, Local
}

final case class SearchOptions(
  query: Option[String],
  location: Option[SearchLocation],
) derives Eq,
      Show

final case class MarketplaceResult(
  name: String,
  source: String,
  description: String,
  installs: Long,
  marketplace: String,
) derives Eq,
      Show

final case class LocalSearchResult(
  skill: Skill,
  score: Int,
) derives Eq,
      Show
