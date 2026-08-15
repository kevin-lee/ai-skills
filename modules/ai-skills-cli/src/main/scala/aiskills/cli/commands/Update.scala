package aiskills.cli.commands

import aiskills.core.{RepoUrl, SkillSourceType}
import aiskills.core.utils.{Dirs, SkillMetadata, SkillNames, Skills, Yaml}
import cats.syntax.all.*
import extras.scala.io.syntax.color.*

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

        // Phase 4: Group git skills by normalized repo URL and clone each repo once
        if gitWithUrl.nonEmpty then {
          val groupedByRepo = gitWithUrl.groupBy {
            case (_, meta) =>
              meta.repoUrl.fold("")(normalizeRepoUrl)
          }

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

                    GitClone.cloneRepoWithUi(
                      cloneUrl,
                      repoSubDir / "repo",
                      preferred = preferred,
                      interactivity = GitClone.Interactivity.Allowed,
                      texts = GitClone.CloneTexts(
                        s"Cloning ${cloneUrl.value}$skillsLabel...",
                        s"Cloned: ${cloneUrl.value}$skillsLabel",
                        s"Clone failed: ${cloneUrl.value}",
                      ),
                    ) match {
                      case Left(_) =>
                        groupSkills.foreach {
                          case (skill, _) =>
                            println(s"Skipped: ${skill.name} (git clone failed)".yellow)
                            cloneFailures += skill.name
                        }

                      case Right(cloned) =>
                        val repoDir = repoSubDir / "repo"

                        groupSkills.foreach {
                          case (skill, meta) =>
                            val subpath   = meta.subpath
                            val sourceDir = subpath.fold(repoDir)(sp => repoDir / os.RelPath(sp))

                            if !os.exists(sourceDir / "SKILL.md") then {
                              println(
                                s"Skipped: ${skill.name} (SKILL.md not found in repo at ${subpath.getOrElse(".")})".yellow
                              )
                              missingRepoSkillFile += skill.name -> subpath.getOrElse(".")
                            } else {
                              updateSkillFromDir(skill.path, sourceDir)
                              val updatedMeta =
                                meta
                                  .withRepoUrl(cloned.url.some)
                                  .withAuthMethod(cloned.method.some)
                                  .withInstalledAt(aiskills.core.utils.isoNow())
                              SkillMetadata.writeSkillMetadata(skill.path, updatedMeta)
                              reapplyRename(skill.path, updatedMeta)
                              val pathLabel   = Dirs.displaySkillsDir(skill.agent, skill.location)
                              println(
                                s"\u2705 Updated: ${skill.name} (${skill.location.toString.toLowerCase}, ${skill.agent.toString}): $pathLabel".green
                              )
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

        val skipped =
          missingMetadataList.length +
            missingLocalSourceList.length +
            missingLocalSkillFileList.length +
            missingRepoUrlList.length +
            missingRepoSkillFileList.length +
            cloneFailuresList.length
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
