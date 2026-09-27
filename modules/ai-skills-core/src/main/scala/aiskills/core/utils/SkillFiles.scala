package aiskills.core.utils

import cats.syntax.all.*

import java.nio.file.{CopyOption, Files, StandardCopyOption}

object SkillFiles {

  /** Copy a skill folder the way `os.copy` does, leaving out every entry named `.git` at any depth, whether it is a
    * directory or a file. A skill never needs Git's data, and a copied `.git` makes Git treat the installed skill
    * as an embedded repository.
    */
  def copyWithoutGit(from: os.Path, to: os.Path, replaceExisting: Boolean): Unit = {
    require(!to.startsWith(from), s"Can't copy a directory into itself: $to is inside $from")
    val options: List[CopyOption] =
      if replaceExisting then List(StandardCopyOption.REPLACE_EXISTING) else Nil
    (from :: os.walk(from, skip = _.last === ".git").toList).foreach { path =>
      Files.copy(path.toNIO, (to / path.relativeTo(from)).toNIO, options*)
    }
  }
}
