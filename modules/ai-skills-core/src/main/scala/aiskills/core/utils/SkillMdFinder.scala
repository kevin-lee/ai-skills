package aiskills.core.utils

import cats.syntax.all.*

import scala.util.Try

object SkillMdFinder {

  /** Collect every SKILL.md under a directory. Uses 'git ls-files' when the directory is a git working tree
    * (honors .gitignore and never enters .git); falls back to a filesystem walk that skips .git otherwise.
    */
  def listSkillMds(dir: os.Path): List[os.Path] =
    listSkillMdsViaGit(dir).getOrElse(listSkillMdsViaFs(dir))

  /** Use `git ls-files` to enumerate every SKILL.md under a git working tree.
    * Returns None when the directory is not a git repo, git is unavailable, or the command fails.
    */
  private def listSkillMdsViaGit(repoDir: os.Path): Option[List[os.Path]] =
    Try {
      val result = os
        .proc("git", "-C", repoDir.toString, "ls-files", "-z", "--cached", "--others", "--exclude-standard")
        .call(check = false, stderr = os.Pipe, mergeErrIntoOut = false)
      if result.exitCode =!= 0 then {
        none[List[os.Path]]
      } else {
        val paths = result
          .out
          .text()
          .split('\u0000')
          .iterator
          .filter(_.nonEmpty)
          .map(segment => repoDir / os.RelPath(segment))
          .filter(p => p.last === "SKILL.md")
          .filter(p => Try(os.exists(p)).getOrElse(false))
          .toList
        paths.some
      }
    }.toOption.flatten

  /** Filesystem fallback: collect every SKILL.md under `dir`, skipping `.git` directories. */
  def listSkillMdsViaFs(dir: os.Path): List[os.Path] = {
    val here     = {
      val md = dir / "SKILL.md"
      if os.exists(md) && Try(os.isFile(md)).getOrElse(false) then List(md) else Nil
    }
    val children = Try(os.list(dir)).toOption.getOrElse(Vector.empty).toList
    val nested   = children.flatMap { entry =>
      if Try(os.isDir(entry)).getOrElse(false) && entry.last =!= ".git" then listSkillMdsViaFs(entry)
      else Nil
    }
    here ++ nested
  }
}
