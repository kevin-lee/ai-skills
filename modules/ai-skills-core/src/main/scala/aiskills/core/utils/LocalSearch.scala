package aiskills.core.utils

import aiskills.core.{LocalSearchResult, Skill}
import cats.syntax.all.*

object LocalSearch {

  /** Fuzzy search installed skills by matching query against name and description.
    *
    * Scoring:
    *  - Exact name match: 100
    *  - Name starts with query: 80
    *  - Name contains query: 60
    *  - Description contains query: 30
    *
    * Returns results sorted by score descending, filtered to score > 0.
    */
  def fuzzySearch(query: String, skills: List[Skill]): List[LocalSearchResult] = {
    val q = query.toLowerCase.trim
    if q.length < 2 then Nil
    else
      skills
        .map { skill =>
          val score = scoreSkill(q, skill)
          LocalSearchResult(skill, score)
        }
        .filter(_.score > 0)
        .sortBy(-_.score)
  }

  private def scoreSkill(query: String, skill: Skill): Int = {
    val name = skill.name.toLowerCase
    val desc = skill.description.toLowerCase

    val nameScore =
      if name === query then 100
      else if name.startsWith(query) then 80
      else if name.contains(query) then 60
      else 0

    val descScore =
      if desc.contains(query) then 30
      else 0

    math.max(nameScore, descScore)
  }
}
