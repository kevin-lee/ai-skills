package aiskills.cli.commands

import aiskills.core.SkillSourceType
import aiskills.core.utils.{SkillMetadata, SkillNames, Skills}
import extras.scala.io.syntax.color.*

import scala.util.{Try, Success, Failure}

object Update:

  /** Update installed skills from their recorded source metadata. */
  def updateSkills(skillNames: List[String]): Unit =
    val requested = SkillNames.normalizeSkillNames(skillNames)
    val skills = Skills.findAllSkills()

    if skills.isEmpty then
      println("No skills installed.\n")
      println("Install skills:")
      println(s"  ${"aiskills install anthropics/skills".cyan}         ${"# Project (default)".dim}")
      println(s"  ${"aiskills install owner/skill --global".cyan}     ${"# Global (advanced)".dim}")
      return

    val targets =
      if requested.nonEmpty then
        val requestedSet = requested.toSet
        val missing = requested.filterNot(name => skills.exists(_.name == name))
        if missing.nonEmpty then
          println(s"Skipping missing skills: ${missing.mkString(", ")}".yellow)
        skills.filter(s => requestedSet.contains(s.name))
      else
        skills

    if targets.isEmpty then
      println("No matching skills to update.".yellow)
      return

    var updated = 0
    var skipped = 0
    val missingMetadata = List.newBuilder[String]
    val missingLocalSource = List.newBuilder[String]
    val missingLocalSkillFile = List.newBuilder[String]
    val missingRepoUrl = List.newBuilder[String]
    val missingRepoSkillFile = List.newBuilder[(String, String)]
    val cloneFailures = List.newBuilder[String]

    for skill <- targets do
      val metadata = SkillMetadata.readSkillMetadata(skill.path)
      metadata match
        case None =>
          println(s"Skipped: ${skill.name} [${skill.agent.toString}] (no source metadata; re-install once to enable updates)".yellow)
          missingMetadata += skill.name
          skipped += 1

        case Some(meta) if meta.sourceType == SkillSourceType.Local =>
          val localPath = meta.localPath.map(os.Path(_))
          localPath match
            case None =>
              println(s"Skipped: ${skill.name} (local source missing)".yellow)
              missingLocalSource += skill.name
              skipped += 1
            case Some(lp) if !os.exists(lp) =>
              println(s"Skipped: ${skill.name} (local source missing)".yellow)
              missingLocalSource += skill.name
              skipped += 1
            case Some(lp) if !os.exists(lp / "SKILL.md") =>
              println(s"Skipped: ${skill.name} (SKILL.md missing at local source)".yellow)
              missingLocalSkillFile += skill.name
              skipped += 1
            case Some(lp) =>
              updateSkillFromDir(skill.path, lp)
              SkillMetadata.writeSkillMetadata(
                skill.path,
                meta.copy(installedAt = aiskills.core.utils.isoNow()),
              )
              println(s"\u2705 Updated: ${skill.name} [${skill.agent.toString}]".green)
              updated += 1

        case Some(meta) =>
          meta.repoUrl match
            case None =>
              println(s"Skipped: ${skill.name} (missing repo URL metadata)".yellow)
              missingRepoUrl += skill.name
              skipped += 1

            case Some(repoUrl) =>
              val tempDir = os.home / s".aiskills-temp-${System.currentTimeMillis()}"
              os.makeDir.all(tempDir)
              try
                print(s"Updating ${skill.name}...")
                Try {
                  os.proc("git", "clone", "--depth", "1", "--quiet", repoUrl, (tempDir / "repo").toString)
                    .call(stderr = os.Pipe)
                } match
                  case Failure(ex) =>
                    println(s" failed")
                    val msg = ex.getMessage
                    if msg.nonEmpty then println(msg.dim)
                    println(s"Skipped: ${skill.name} (git clone failed)".yellow)
                    cloneFailures += skill.name
                    skipped += 1

                  case Success(_) =>
                    val repoDir = tempDir / "repo"
                    val subpath = meta.subpath.filter(s => s.nonEmpty && s != ".")
                    val sourceDir = subpath.fold(repoDir)(sp => repoDir / os.RelPath(sp))

                    if !os.exists(sourceDir / "SKILL.md") then
                      println(s" failed")
                      println(s"Skipped: ${skill.name} (SKILL.md not found in repo at ${subpath.getOrElse(".")})".yellow)
                      missingRepoSkillFile += ((skill.name, subpath.getOrElse(".")))
                      skipped += 1
                    else
                      updateSkillFromDir(skill.path, sourceDir)
                      SkillMetadata.writeSkillMetadata(
                        skill.path,
                        meta.copy(installedAt = aiskills.core.utils.isoNow()),
                      )
                      println(s" done")
                      println(s"\u2705 Updated: ${skill.name} [${skill.agent.toString}]".green)
                      updated += 1
              finally
                os.remove.all(tempDir)

    println(s"Summary: $updated updated, $skipped skipped (${targets.length} total)".dim)

    val missingMetadataList = missingMetadata.result()
    if missingMetadataList.nonEmpty then
      println(s"Missing source metadata (${missingMetadataList.length}): ${missingMetadataList.mkString(", ")}".yellow)
      println("Re-install these skills once to enable updates (e.g., `aiskills install <source>`).".dim)

    val missingLocalSourceList = missingLocalSource.result()
    if missingLocalSourceList.nonEmpty then
      println(s"Local source missing (${missingLocalSourceList.length}): ${missingLocalSourceList.mkString(", ")}".yellow)

    val missingLocalSkillFileList = missingLocalSkillFile.result()
    if missingLocalSkillFileList.nonEmpty then
      println(s"Local SKILL.md missing (${missingLocalSkillFileList.length}): ${missingLocalSkillFileList.mkString(", ")}".yellow)

    val missingRepoUrlList = missingRepoUrl.result()
    if missingRepoUrlList.nonEmpty then
      println(s"Missing repo URL metadata (${missingRepoUrlList.length}): ${missingRepoUrlList.mkString(", ")}".yellow)

    val missingRepoSkillFileList = missingRepoSkillFile.result()
    if missingRepoSkillFileList.nonEmpty then
      val formatted = missingRepoSkillFileList.map((n, s) => s"$n ($s)").mkString(", ")
      println(s"Repo SKILL.md missing (${missingRepoSkillFileList.length}): $formatted".yellow)

    val cloneFailuresList = cloneFailures.result()
    if cloneFailuresList.nonEmpty then
      println(s"Clone failed (${cloneFailuresList.length}): ${cloneFailuresList.mkString(", ")}".yellow)

  private def updateSkillFromDir(targetPath: os.Path, sourceDir: os.Path): Unit =
    val targetDir = targetPath / os.up
    os.makeDir.all(targetDir)

    if !isPathInside(targetPath, targetDir) then
      System.err.println("Security error: Installation path outside target directory".red)
      sys.exit(1)

    os.remove.all(targetPath)
    os.copy(sourceDir, targetPath)

  private def isPathInside(target: os.Path, parent: os.Path): Boolean =
    target.startsWith(parent)
