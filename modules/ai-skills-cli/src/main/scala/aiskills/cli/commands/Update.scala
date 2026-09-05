package aiskills.cli.commands

import aiskills.core.{*, given}
import GitClone.{CloneError, CloneRequest, CloneSuccess, Interactivity}
import cats.*
import cats.derived.*
import cue4s.*
import scala.util.Try
import aiskills.core.utils.{Dirs, SkillMetadata, SkillNames, Skills, Yaml}
import cats.syntax.all.*
import extras.scala.io.syntax.color.*

object Update {

  final case class RepoBranchKey(repo: String, branch: Option[GitBranch]) derives Eq, Show

  enum SwitchBranchChoice derives Eq, Show {
    case KeepBranch, UseDefaultBranch
  }

  final case class ResolvedUpdateSource(cloned: CloneSuccess, branch: Option[GitBranch]) derives Eq, Show

  enum UpdateSourceError derives Eq, Show {
    case CloneFailed(error: CloneError)
    case BranchRetained(branch: GitBranch)
  }

  enum GitUpdateError derives Eq, Show {
    case PreparationFailed(detail: String)
    case ReplacementFailed(detail: String)
    case RollbackFailed(backupPath: os.Path, detail: String)
  }

  private[commands] def groupGitSkills(
    skills: List[(Skill, SkillSourceMetadata)],
  ): Map[RepoBranchKey, List[(Skill, SkillSourceMetadata)]] = {
    skills.groupBy { case (_, meta) => RepoBranchKey(meta.repoUrl.fold("")(normalizeRepoUrl), meta.branch) }
  }

  private[commands] def resolveUpdateSource(
    request: CloneRequest,
    labels: List[String],
    clone: CloneRequest => Either[CloneError, CloneSuccess],
    askSwitch: (RepoUrl, GitBranch, List[String]) => SwitchBranchChoice,
  ): Either[UpdateSourceError, ResolvedUpdateSource] = {
    clone(request) match {
      case Right(cloned) => ResolvedUpdateSource(cloned, request.branch).asRight
      case Left(error @ (CloneError.Failed(_) | CloneError.InvalidBranch(_, _))) =>
        UpdateSourceError.CloneFailed(error).asLeft
      case Left(CloneError.MissingBranch(branch, strategy)) =>
        val choice = request.interactivity match {
          case Interactivity.NotAllowed => SwitchBranchChoice.KeepBranch
          case Interactivity.Allowed => askSwitch(request.repoUrl, branch, labels)
        }
        choice match {
          case SwitchBranchChoice.KeepBranch => UpdateSourceError.BranchRetained(branch).asLeft
          case SwitchBranchChoice.UseDefaultBranch =>
            val fallback = request.copy(
              targetPath = request.targetPath / os.up / "default-repo",
              branch = none[GitBranch],
              preferred = strategy.method.some,
              texts = GitClone.CloneTexts(
                s"Cloning ${request.repoUrl.value} (default branch)...",
                s"Cloned: ${request.repoUrl.value} (default branch)",
                s"Clone failed: ${request.repoUrl.value} (default branch)"
              ),
            )
            clone(fallback)
              .left
              .map(UpdateSourceError.CloneFailed(_))
              .map(cloned => ResolvedUpdateSource(cloned, none[GitBranch]))
        }
    }
  }

  private def askSwitchToDefaultBranch(
    repoUrl: RepoUrl,
    branch: GitBranch,
    labels: List[String]
  ): SwitchBranchChoice = {
    println(s"Repository: ${repoUrl.value}")
    labels.foreach(label => println(s"  $label"))
    val keep   = "Keep branch - skip these updates"
    val switch = "Switch to default branch"
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice(
        s"Branch '${branch.value}' no longer exists. Switch these installations to the repository's default branch for this and future updates?",
        List(keep, switch),
      ) match {
        case Completion.Finished(selected) =>
          if (selected === switch) SwitchBranchChoice.UseDefaultBranch else SwitchBranchChoice.KeepBranch
        case Completion.Fail(CompletionError.Interrupted) =>
          println("Cancelled - keeping the recorded branch".yellow)
          SwitchBranchChoice.KeepBranch
        case Completion.Fail(CompletionError.Error(_)) => SwitchBranchChoice.KeepBranch
      }
    }
  }

  private def failureDetail(ex: Throwable): String = Option(ex.getMessage).getOrElse(ex.toString)

  private[commands] def replaceGitUpdate(
    targetPath: os.Path,
    candidate: os.Path,
    backup: os.Path,
    move: (os.Path, os.Path) => Either[String, Unit],
  ): Either[GitUpdateError, Unit] = {
    move(targetPath, backup).left.map(GitUpdateError.ReplacementFailed(_)).flatMap { _ =>
      move(candidate, targetPath) match {
        case Right(_) => ().asRight
        case Left(detail) =>
          move(backup, targetPath) match {
            case Right(_) => GitUpdateError.ReplacementFailed(detail).asLeft
            case Left(rollbackDetail) =>
              GitUpdateError.RollbackFailed(backup, s"$detail. Rollback failed: $rollbackDetail").asLeft
          }
      }
    }
  }

  private[commands] def installGitUpdate(
    targetPath: os.Path,
    sourceDir: os.Path,
    metadata: SkillSourceMetadata,
  ): Either[GitUpdateError, Unit] = {
    val staging = Try(os.Path(java.nio.file.Files.createTempDirectory((targetPath / os.up).toNIO, ".aiskills-update-")))
      .toEither
      .left
      .map(ex => GitUpdateError.PreparationFailed(failureDetail(ex)))
    staging.flatMap { stage =>
      val candidate = stage / "candidate"
      val backup    = stage / "backup"
      val prepared  = Try {
        os.copy(sourceDir, candidate)
        reapplyRename(candidate, metadata)
        SkillMetadata.writeSkillMetadata(candidate, metadata)
      }.toEither.left.map(ex => GitUpdateError.PreparationFailed(failureDetail(ex)))
      val result    = prepared.flatMap { _ =>
        replaceGitUpdate(
          targetPath,
          candidate,
          backup,
          (from, to) => Try(os.move(from, to)).toEither.left.map(failureDetail)
        )
      }
      result match {
        case Left(GitUpdateError.RollbackFailed(_, _)) => ()
        case Left(GitUpdateError.PreparationFailed(_) | GitUpdateError.ReplacementFailed(_)) | Right(_) =>
          val _ = Try(os.remove.all(stage))
      }
      result
    }
  }

  /** Update installed skills from their recorded source metadata. */
  def updateSkills(skillNames: List[String]): Unit = {
    val requested = SkillNames.normalizeSkillNames(skillNames)
    val skills    = Skills.findAllSkills()

    if skills.isEmpty then {
      println("No skills installed.\n")
      println("Install skills:")
      println(s"  ${"aiskills install anthropics/skills".cyan}         ${"# Project (default)".dim}")
      println(s"  ${"aiskills install owner/skill --global".cyan}     ${"# Global (advanced)".dim}")
    } else {
      val targets =
        if requested.nonEmpty then {
          val requestedSet = requested.toSet
          val missing      = requested.filterNot(name => skills.exists(_.name === name))
          if missing.nonEmpty then println(s"Skipping missing skills: ${missing.mkString(", ")}".yellow)
          else ()
          skills.filter(s => requestedSet.contains(s.name))
        } else
          skills

      if targets.isEmpty then println("No matching skills to update.".yellow)
      else {
        val missingMetadata       = List.newBuilder[String]
        val missingLocalSource    = List.newBuilder[String]
        val missingLocalSkillFile = List.newBuilder[String]
        val missingRepoUrl        = List.newBuilder[String]
        val missingRepoSkillFile  = List.newBuilder[(String, String)]
        val cloneFailures         = List.newBuilder[String]
        val retainedBranches      = List.newBuilder[String]
        val updateFailures        = List.newBuilder[String]
        val interactivity         = if (GitClone.isStdinTty) Interactivity.Allowed else Interactivity.NotAllowed

        aiskills.cli.TempDirCleanup.ensureAtexitRegistered()

        // Phase 1: Classify targets by source type
        val classified = targets.map(skill => (skill, SkillMetadata.readSkillMetadata(skill.path)))

        val (noMeta, withMeta)       = classified.partition { case (_, meta) => meta.isEmpty }
        val withMetaFlat             = withMeta.collect { case (s, Some(m)) => (s, m) }
        val (localSkills, gitSkills) =
          withMetaFlat.partition { case (_, meta) => meta.sourceType === SkillSourceType.Local }
        val (gitWithUrl, gitNoUrl)   =
          gitSkills.partition { case (_, meta) => meta.repoUrl.isDefined }

        // Phase 2: Skip skills with no metadata or missing repo URL
        noMeta.foreach {
          case (skill, _) =>
            val pathLabel = Dirs.displaySkillsDir(skill.agent, skill.location)
            println(
              s"Skipped: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel (no source metadata; re-install once to enable updates)".yellow
            )
            missingMetadata += skill.name
        }

        gitNoUrl.foreach {
          case (skill, _) =>
            println(s"Skipped: ${skill.name} (missing repo URL metadata)".yellow)
            missingRepoUrl += skill.name
        }

        // Phase 3: Process local skills individually
        localSkills.foreach {
          case (skill, meta) =>
            val localPath = meta.localPath.map(os.Path(_))
            localPath match {
              case None =>
                println(s"Skipped: ${skill.name} (local source missing)".yellow)
                missingLocalSource += skill.name
              case Some(lp) if !os.exists(lp) =>
                println(s"Skipped: ${skill.name} (local source missing)".yellow)
                missingLocalSource += skill.name
              case Some(lp) if !os.exists(lp / "SKILL.md") =>
                println(s"Skipped: ${skill.name} (SKILL.md missing at local source)".yellow)
                missingLocalSkillFile += skill.name
              case Some(lp) =>
                updateSkillFromDir(skill.path, lp)
                val updatedMeta = meta.withInstalledAt(aiskills.core.utils.isoNow())
                SkillMetadata.writeSkillMetadata(skill.path, updatedMeta)
                reapplyRename(skill.path, updatedMeta)
                val pathLabel   = Dirs.displaySkillsDir(skill.agent, skill.location)
                println(
                  s"\u2705 Updated: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel".green
                )
            }
        }

        // Phase 4: Clone each selected repository and branch group.
        if gitWithUrl.nonEmpty then {
          val groupedByRepo = groupGitSkills(gitWithUrl)

          val parentTempDir = aiskills.cli.TempDirCleanup.createTempDir()

          try {
            groupedByRepo.iterator.zipWithIndex.foreach {
              case ((_, groupSkills), idx) =>
                groupSkills.headOption.foreach {
                  case (firstSkill, firstMeta) =>
                    val cloneUrl    = firstMeta.repoUrl.getOrElse(RepoUrl(firstSkill.name))
                    val repoSubDir  = parentTempDir / s"repo-$idx"
                    os.makeDir.all(repoSubDir)
                    val skillNames  = groupSkills.map { case (skill, _) => skill.name }
                    val skillsLabel = if skillNames.length > 1 then s" (${skillNames.length} skills)" else ""

                    // A recorded auth method is a hint: it only moves that method to the front of the chain.
                    val preferred = groupSkills.collectFirst {
                      case (_, meta) if meta.authMethod.isDefined => meta.authMethod
                    }.flatten

                    val branchLabel = firstMeta.branch.fold("default branch")(b => s"branch: ${b.value}")
                    val request     = CloneRequest(
                      repoUrl = cloneUrl,
                      targetPath = repoSubDir / "repo",
                      branch = firstMeta.branch,
                      preferred = preferred,
                      interactivity = interactivity,
                      texts = GitClone.CloneTexts(
                        s"Cloning ${cloneUrl.value} ($branchLabel)$skillsLabel...",
                        s"Cloned: ${cloneUrl.value} ($branchLabel)$skillsLabel",
                        s"Clone failed: ${cloneUrl.value} ($branchLabel)",
                      ),
                    )
                    val labels      = groupSkills.map {
                      case (skill, _) =>
                        s"${skill.name} (${skill.agent.toString}, ${skill.location.toString.toLowerCase}): ${skill.path}"
                    }
                    resolveUpdateSource(request, labels, GitClone.cloneRepoWithUi, askSwitchToDefaultBranch) match {
                      case Left(UpdateSourceError.BranchRetained(branch)) =>
                        groupSkills.foreach {
                          case (skill, _) =>
                            println(
                              s"Skipped: ${skill.name} (branch '${branch.value}' missing - selection retained)".yellow
                            )
                            retainedBranches += skill.name
                        }
                      case Left(UpdateSourceError.CloneFailed(_)) =>
                        groupSkills.foreach {
                          case (skill, _) =>
                            println(s"Skipped: ${skill.name} (git clone failed)".yellow)
                            cloneFailures += skill.name
                        }
                      case Right(resolved) =>
                        val repoDir =
                          if (resolved.branch === request.branch) request.targetPath else repoSubDir / "default-repo"
                        groupSkills.foreach {
                          case (skill, meta) =>
                            val sourceDir = meta.subpath.fold(repoDir)(sp => repoDir / os.RelPath(sp))
                            if (!os.exists(sourceDir / "SKILL.md")) {
                              println(
                                s"Skipped: ${skill.name} (SKILL.md not found in repo at ${meta.subpath.getOrElse(".")})".yellow
                              )
                              missingRepoSkillFile += skill.name -> meta.subpath.getOrElse(".")
                            } else {
                              val updatedMeta = meta
                                .withRepoUrl(resolved.cloned.url.some)
                                .withAuthMethod(resolved.cloned.method.some)
                                .withBranch(resolved.branch)
                                .withInstalledAt(aiskills.core.utils.isoNow())
                              installGitUpdate(skill.path, sourceDir, updatedMeta) match {
                                case Right(_) =>
                                  val pathLabel = Dirs.displaySkillsDir(skill.agent, skill.location)
                                  println(
                                    s"✅ Updated: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel".green
                                  )
                                case Left(error) =>
                                  val detail = error match {
                                    case GitUpdateError.PreparationFailed(message) => s"Preparation failed: $message"
                                    case GitUpdateError.ReplacementFailed(message) => s"Replacement failed: $message"
                                    case GitUpdateError.RollbackFailed(backup, message) =>
                                      s"$message. Recover the installation from: $backup"
                                  }
                                  println(s"Skipped: ${skill.name} ($detail)".yellow)
                                  updateFailures += skill.name
                              }
                            }
                        }
                    }
                }
            }
          } finally {
            aiskills.cli.TempDirCleanup.safeRemoveAll(parentTempDir)
            aiskills.cli.TempDirCleanup.unregister(parentTempDir)
          }
        } else ()

        // Phase 5: Summary
        val missingMetadataList       = missingMetadata.result()
        val missingLocalSourceList    = missingLocalSource.result()
        val missingLocalSkillFileList = missingLocalSkillFile.result()
        val missingRepoUrlList        = missingRepoUrl.result()
        val missingRepoSkillFileList  = missingRepoSkillFile.result()
        val cloneFailuresList         = cloneFailures.result()
        val retainedBranchesList      = retainedBranches.result()
        val updateFailuresList        = updateFailures.result()

        val skipped =
          missingMetadataList.length +
            missingLocalSourceList.length +
            missingLocalSkillFileList.length +
            missingRepoUrlList.length +
            missingRepoSkillFileList.length +
            cloneFailuresList.length + retainedBranchesList.length + updateFailuresList.length
        val updated = targets.length - skipped

        println(s"Summary: $updated updated, $skipped skipped (${targets.length} total)".dim)

        if missingMetadataList.nonEmpty then {
          println(
            s"Missing source metadata (${missingMetadataList.length}): ${missingMetadataList.mkString(", ")}".yellow
          )
          println("Re-install these skills once to enable updates (e.g., `aiskills install <source>`).".dim)
        } else ()

        if missingLocalSourceList.nonEmpty then println(
          s"Local source missing (${missingLocalSourceList.length}): ${missingLocalSourceList.mkString(", ")}".yellow
        )
        else ()

        if missingLocalSkillFileList.nonEmpty then println(
          s"Local SKILL.md missing (${missingLocalSkillFileList.length}): ${missingLocalSkillFileList.mkString(", ")}".yellow
        )
        else ()

        if missingRepoUrlList.nonEmpty then println(
          s"Missing repo URL metadata (${missingRepoUrlList.length}): ${missingRepoUrlList.mkString(", ")}".yellow
        )
        else ()

        if missingRepoSkillFileList.nonEmpty then {
          val formatted = missingRepoSkillFileList.map { case (name, sub) => s"$name ($sub)" }.mkString(", ")
          println(s"Repo SKILL.md missing (${missingRepoSkillFileList.length}): $formatted".yellow)
        } else ()

        if (retainedBranchesList.nonEmpty) {
          println(
            s"Missing branch - selection retained (${retainedBranchesList.length}): ${retainedBranchesList.mkString(", ")}".yellow
          )
        } else { () }
        if (updateFailuresList.nonEmpty) {
          println(s"Update failed (${updateFailuresList.length}): ${updateFailuresList.mkString(", ")}".yellow)
        } else { () }

        if cloneFailuresList.nonEmpty then println(
          s"Clone failed (${cloneFailuresList.length}): ${cloneFailuresList.mkString(", ")}".yellow
        )
        else ()
      }
    }
  }

  private def updateSkillFromDir(targetPath: os.Path, sourceDir: os.Path): Unit = {
    val targetDir = targetPath / os.up
    os.makeDir.all(targetDir)

    if !isPathInside(targetPath, targetDir) then {
      System.err.println("Security error: Installation path outside target directory".red)
      sys.exit(1)
    } else {
      os.remove.all(targetPath)
      os.copy(sourceDir, targetPath)
    }
  }

  private def isPathInside(target: os.Path, parent: os.Path): Boolean =
    target.startsWith(parent)

  /** Re-apply the renamed name to SKILL.md after an update replaces it with the original source. */
  private def reapplyRename(skillPath: os.Path, meta: aiskills.core.SkillSourceMetadata): Unit =
    meta.name.foreach { renamedName =>
      val skillMdPath = skillPath / "SKILL.md"
      if os.exists(skillMdPath) then {
        val content = os.read(skillMdPath)
        val updated = Yaml.replaceYamlField(content, "name", renamedName)
        os.write.over(skillMdPath, updated)
      } else ()
    }

  /** Normalize a Git repository URL to a canonical form for grouping.
    * Strips protocol, trailing slashes, and .git suffix. Lowercases.
    * {{{
    * "https://github.com/owner/repo.git" -> "github.com/owner/repo"
    * "git@github.com:owner/repo.git"     -> "github.com/owner/repo"
    * }}}
    */
  def normalizeRepoUrl(url: RepoUrl): String = {
    val cleaned = url.value.trim.stripSuffix("/").stripSuffix(".git").toLowerCase
    if cleaned.startsWith("git@") then {
      val afterAt  = cleaned.stripPrefix("git@")
      val colonIdx = afterAt.indexOf(':')
      if colonIdx > 0 then {
        val host = afterAt.substring(0, colonIdx)
        val path = afterAt.substring(colonIdx + 1)
        s"$host/$path"
      } else cleaned
    } else if cleaned.startsWith("https://") then cleaned.stripPrefix("https://")
    else if cleaned.startsWith("http://") then cleaned.stripPrefix("http://")
    else if cleaned.startsWith("git://") then cleaned.stripPrefix("git://")
    else cleaned
  }
}
