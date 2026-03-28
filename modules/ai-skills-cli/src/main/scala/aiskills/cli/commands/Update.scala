package aiskills.cli.commands

import aiskills.core.SkillSourceType
import aiskills.core.utils.{Dirs, SkillMetadata, SkillNames, Skills}
import cats.syntax.all.*
import extras.scala.io.syntax.color.*

import just.spinner.*

import scala.util.{Failure, Success, Try}

object Update {

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

        aiskills.cli.TempDirCleanup.ensureAtexitRegistered()

        val (updated, skipped) = targets.foldLeft((0, 0)) {
          case ((upd, skp), skill) =>
            val metadata = SkillMetadata.readSkillMetadata(skill.path)
            metadata match {
              case None =>
                val pathLabel = Dirs.displaySkillsDir(skill.agent, skill.location)
                println(
                  s"Skipped: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel (no source metadata; re-install once to enable updates)".yellow
                )
                missingMetadata += skill.name
                (upd, skp + 1)

              case Some(meta) if meta.sourceType === SkillSourceType.Local =>
                val localPath = meta.localPath.map(os.Path(_))
                localPath match {
                  case None =>
                    println(s"Skipped: ${skill.name} (local source missing)".yellow)
                    missingLocalSource += skill.name
                    (upd, skp + 1)
                  case Some(lp) if !os.exists(lp) =>
                    println(s"Skipped: ${skill.name} (local source missing)".yellow)
                    missingLocalSource += skill.name
                    (upd, skp + 1)
                  case Some(lp) if !os.exists(lp / "SKILL.md") =>
                    println(s"Skipped: ${skill.name} (SKILL.md missing at local source)".yellow)
                    missingLocalSkillFile += skill.name
                    (upd, skp + 1)
                  case Some(lp) =>
                    updateSkillFromDir(skill.path, lp)
                    SkillMetadata.writeSkillMetadata(
                      skill.path,
                      meta.copy(installedAt = aiskills.core.utils.isoNow()),
                    )
                    val pathLabel = Dirs.displaySkillsDir(skill.agent, skill.location)
                    println(
                      s"\u2705 Updated: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel".green
                    )
                    (upd + 1, skp)
                }

              case Some(meta) =>
                meta.repoUrl match {
                  case None =>
                    println(s"Skipped: ${skill.name} (missing repo URL metadata)".yellow)
                    missingRepoUrl += skill.name
                    (upd, skp + 1)

                  case Some(repoUrl) =>
                    val tempDir = os.home / s".aiskills-temp-${System.currentTimeMillis()}"
                    os.makeDir.all(tempDir)
                    aiskills.cli.TempDirCleanup.register(tempDir)
                    try {
                      val spinner = Spinner.createDefaultSideEffect(
                        SpinnerConfig
                          .default
                          .withText(s"Updating ${skill.name}...")
                          .withColor(Color.cyan)
                          .withIndent(2),
                      )
                      val _       = spinner.start()
                      Try {
                        os.proc("git", "clone", "--depth", "1", "--quiet", repoUrl, (tempDir / "repo").toString)
                          .call(stderr = os.Pipe)
                      } match {
                        case Failure(ex) =>
                          val _   = spinner.fail(Some(s"Clone failed: ${skill.name}"))
                          val msg = ex.getMessage
                          if msg.nonEmpty then println(msg.dim) else ()
                          println(s"Skipped: ${skill.name} (git clone failed)".yellow)
                          cloneFailures += skill.name
                          (upd, skp + 1)

                        case Success(_) =>
                          val repoDir   = tempDir / "repo"
                          val subpath   = meta.subpath.filter(s => s.nonEmpty && s =!= ".")
                          val sourceDir = subpath.fold(repoDir)(sp => repoDir / os.RelPath(sp))

                          if !os.exists(sourceDir / "SKILL.md") then {
                            val _ = spinner.fail(Some(s"Failed: ${skill.name}"))
                            println(
                              s"Skipped: ${skill.name} (SKILL.md not found in repo at ${subpath.getOrElse(".")})".yellow
                            )
                            missingRepoSkillFile += skill.name -> subpath.getOrElse(".")
                            (upd, skp + 1)
                          } else {
                            updateSkillFromDir(skill.path, sourceDir)
                            SkillMetadata.writeSkillMetadata(
                              skill.path,
                              meta.copy(installedAt = aiskills.core.utils.isoNow()),
                            )
                            val pathLabel = Dirs.displaySkillsDir(skill.agent, skill.location)
                            val _         = spinner.succeed(
                              Some(
                                s"Updated: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel"
                              )
                            )
                            (upd + 1, skp)
                          }
                      }
                    } finally {
                      aiskills.cli.TempDirCleanup.safeRemoveAll(tempDir)
                      aiskills.cli.TempDirCleanup.unregister()
                    }
                }
            }
        }

        println(s"Summary: $updated updated, $skipped skipped (${targets.length} total)".dim)

        val missingMetadataList = missingMetadata.result()
        if missingMetadataList.nonEmpty then {
          println(
            s"Missing source metadata (${missingMetadataList.length}): ${missingMetadataList.mkString(", ")}".yellow
          )
          println("Re-install these skills once to enable updates (e.g., `aiskills install <source>`).".dim)
        } else ()

        val missingLocalSourceList = missingLocalSource.result()
        if missingLocalSourceList.nonEmpty then println(
          s"Local source missing (${missingLocalSourceList.length}): ${missingLocalSourceList.mkString(", ")}".yellow
        )
        else ()

        val missingLocalSkillFileList = missingLocalSkillFile.result()
        if missingLocalSkillFileList.nonEmpty then println(
          s"Local SKILL.md missing (${missingLocalSkillFileList.length}): ${missingLocalSkillFileList.mkString(", ")}".yellow
        )
        else ()

        val missingRepoUrlList = missingRepoUrl.result()
        if missingRepoUrlList.nonEmpty then println(
          s"Missing repo URL metadata (${missingRepoUrlList.length}): ${missingRepoUrlList.mkString(", ")}".yellow
        )
        else ()

        val missingRepoSkillFileList = missingRepoSkillFile.result()
        if missingRepoSkillFileList.nonEmpty then {
          val formatted = missingRepoSkillFileList.map((n, s) => s"$n ($s)").mkString(", ")
          println(s"Repo SKILL.md missing (${missingRepoSkillFileList.length}): $formatted".yellow)
        } else ()

        val cloneFailuresList = cloneFailures.result()
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
}
