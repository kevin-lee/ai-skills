package aiskills.core.utils

import aiskills.core.{ContentHash, GitCommitHash, given}
import cats.*
import cats.derived.*
import cats.syntax.all.*

import scala.util.Try

enum SkillHashError derives Eq, Show {
  case NotADirectory(path: os.Path)
  case ContainsSymlink(path: os.Path)
  case CommandFailed(command: String, detail: String)
  case UnexpectedOutput(command: String, output: String)
}

object SkillHash {

  /** Operating system files left out of a skill folder's hash at any depth. */
  val ExcludedFileNames: List[String] = List(".DS_Store", "Thumbs.db", "desktop.ini")

  /** Git tree hash of a plain folder, as `git write-tree` would record it, leaving out the top-level
    * `.aiskills.json` and `ExcludedFileNames`. `.git` is never included. Line endings are normalized,
    * so CRLF and LF checkouts hash the same.
    */
  def directoryHash(dir: os.Path): Either[SkillHashError, ContentHash] =
    if !os.isDir(dir) then SkillHashError.NotADirectory(dir).asLeft
    else
      Try(os.temp.dir(prefix = "aiskills-hash-", deleteOnExit = false))
        .toEither
        .leftMap(ex => SkillHashError.CommandFailed("create temporary directory", failureDetail(ex)))
        .flatMap { tmp =>
          val gitDir           = tmp / "objects.git"
          val indexFile        = tmp / "index"
          val env              = Map("GIT_INDEX_FILE" -> indexFile.toString)
          val excludePathspecs =
            s":(exclude)${SkillMetadata.SkillMetadataFile}" :: ExcludedFileNames.map(name => s":(exclude,glob)**/$name")

          val result = for {
            _      <- runGit(List("init", "--bare", "--quiet", gitDir.toString), tmp, Map.empty)
            _      <- runGit(
                        List(
                          "-c",
                          "core.autocrlf=input",
                          "-c",
                          "core.safecrlf=false",
                          "-c",
                          "core.fsmonitor=false",
                          s"--git-dir=$gitDir",
                          s"--work-tree=$dir",
                          "add",
                          "--all",
                          "--force",
                          "--",
                          ".",
                        ) ++ excludePathspecs,
                        dir,
                        env,
                      )
            output <- runGit(List(s"--git-dir=$gitDir", "write-tree"), tmp, env)
            hash   <- parseHash("git write-tree", output)
          } yield ContentHash(hash)

          val _ = Try(os.remove.all(tmp))
          result
        }

  /** Git tree hash of a skill folder in a cloned repository. `None` means the repository root. */
  def gitTreeHash(repoDir: os.Path, subpath: Option[String]): Either[SkillHashError, ContentHash] = {
    val revision = subpath.fold("HEAD^{tree}")(sp => s"HEAD:${sp.stripSuffix("/")}")
    runGit(List("rev-parse", "--verify", revision), repoDir, Map.empty)
      .flatMap(parseHash(s"git rev-parse --verify $revision", _))
      .map(ContentHash(_))
  }

  /** The commit checked out in a cloned repository. */
  def gitCommit(repoDir: os.Path): Either[SkillHashError, GitCommitHash] =
    runGit(List("rev-parse", "--verify", "HEAD"), repoDir, Map.empty)
      .flatMap(parseHash("git rev-parse --verify HEAD", _))
      .map(GitCommitHash(_))

  /** Whether a folder contains a symbolic link. A folder that cannot be walked counts as containing one. */
  def containsSymlink(dir: os.Path): Boolean =
    Try(os.walk(dir, skip = _.last === ".git").exists(os.isLink(_))).getOrElse(true)

  /** The version of a local source folder. A folder with a symbolic link has no version, because the link may
    * point outside the folder.
    */
  def sourceDirectoryHash(dir: os.Path): Either[SkillHashError, ContentHash] =
    if containsSymlink(dir) then SkillHashError.ContainsSymlink(dir).asLeft
    else directoryHash(dir)

  /** The version of a skill folder in a cloned repository. A folder with a symbolic link has no version, because
    * the link may point outside the folder.
    */
  def sourceGitTreeHash(repoDir: os.Path, subpath: Option[String]): Either[SkillHashError, ContentHash] = {
    val folder = subpath.fold(repoDir)(sp => repoDir / os.RelPath(sp))
    if containsSymlink(folder) then SkillHashError.ContainsSymlink(folder).asLeft
    else gitTreeHash(repoDir, subpath)
  }

  private def runGit(args: List[String], cwd: os.Path, env: Map[String, String]): Either[SkillHashError, String] = {
    val command = ("git" :: args).mkString(" ")
    Try(os.proc("git", args).call(cwd = cwd, env = env, stdout = os.Pipe, stderr = os.Pipe, check = false))
      .toEither
      .leftMap(ex => SkillHashError.CommandFailed(command, failureDetail(ex)))
      .flatMap { result =>
        if result.exitCode === 0 then result.out.text().asRight
        else SkillHashError.CommandFailed(command, result.err.text().trim).asLeft
      }
  }

  private def parseHash(command: String, output: String): Either[SkillHashError, String] = {
    val value = output.trim.toLowerCase
    Either.cond(
      value.forall(c =>
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
      ) && (value.length === 40 || value.length === 64),
      value,
      SkillHashError.UnexpectedOutput(command, output),
    )
  }

  private def failureDetail(ex: Throwable): String = Option(ex.getMessage).getOrElse(ex.toString)
}
