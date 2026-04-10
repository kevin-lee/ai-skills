package aiskills.cli.commands

import OverwritePrompt.{BulkDecision, OverwriteChoice}
import aiskills.cli.CliDefaults
import aiskills.core.utils.{AgentsMd, MarketplaceSkills, SkillMetadata, Yaml}
import aiskills.core.*
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*
import just.spinner.*

import scala.util.{Failure, Success, Try}

object Install {

  /** Check if the source is a local path. */
  def isLocalPath(source: String): Boolean =
    source.startsWith("/") ||
      source.startsWith("./") ||
      source.startsWith("../") ||
      source.startsWith("~/") ||
      source.startsWith("$HOME/")

  /** Check if the source is a git URL (SSH, git://, or HTTPS). */
  def isGitUrl(source: String): Boolean =
    source.startsWith("git@") ||
      source.startsWith("git://") ||
      source.startsWith("http://") ||
      source.startsWith("https://") ||
      source.endsWith(".git")

  /** Check if the URL is a GitHub HTTPS URL. */
  def isGitHubHttpsUrl(url: String): Boolean =
    url.startsWith("https://github.com/")

  /** Convert a GitHub HTTPS URL to the equivalent SSH URL. */
  def gitHubHttpsToSsh(url: String): String = {
    val path = url.stripPrefix("https://github.com/").stripSuffix("/").stripSuffix(".git")
    s"git@github.com:$path.git"
  }

  /** Clone a repo, falling back to SSH if the HTTPS GitHub URL fails.
    *
    * The HTTPS attempt uses `-c credential.helper=` to disable credential
    * helpers and `GIT_TERMINAL_PROMPT=0` so the clone fails fast instead
    * of prompting for a password.
    *
    * @return the URL that actually succeeded (may differ from `repoUrl` when fallback was used)
    */
  def cloneWithFallback(repoUrl: String, targetPath: String): String = {
    val noPromptEnv = Map("GIT_TERMINAL_PROMPT" -> "0")
    Try {
      os.proc("git", "-c", "credential.helper=", "clone", "--depth", "1", "--quiet", repoUrl, targetPath)
        .call(stderr = os.Pipe, env = noPromptEnv)
    } match {
      case Success(_) => repoUrl
      case Failure(_) if isGitHubHttpsUrl(repoUrl) =>
        val sshUrl = gitHubHttpsToSsh(repoUrl)
        Try {
          os.proc("git", "clone", "--depth", "1", "--quiet", sshUrl, targetPath)
            .call(stderr = os.Pipe)
        } match {
          case Success(_) => sshUrl
          case Failure(sshEx) => throw sshEx // scalafix:ok DisableSyntax.throw
        }
      case Failure(ex) => throw ex // scalafix:ok DisableSyntax.throw
    }
  }

  /** Extract the repo name from the git URL. */
  def getRepoName(repoUrl: String): Option[String] = {
    val cleaned = repoUrl.stripSuffix(".git")
    cleaned.split("/").lastOption.flatMap { lastPart =>
      val maybeRepo = if lastPart.contains(":") then lastPart.split(":").lastOption else lastPart.some
      maybeRepo.filter(_.nonEmpty)
    }
  }

  /** Expand ~ to home directory. */
  def expandPath(source: String): os.Path =
    if source.startsWith("~/") || source.startsWith("$HOME/") then os.home / os.RelPath(source.drop(2))
    else
      os.Path(source, os.pwd)

  /** Ensure target path stays within target directory. */
  def isPathInside(targetPath: os.Path, targetDir: os.Path): Boolean =
    targetPath.startsWith(targetDir)

  /** Get directory size in bytes. */
  def getDirectorySize(dirPath: os.Path): Long =
    if !os.exists(dirPath) then 0L
    else
      os.walk(dirPath)
        .filter(os.isFile(_))
        .map(p => os.stat(p).size)
        .sum

  /** Format bytes to human-readable size. */
  def formatSize(bytes: Long): String =
    if bytes < 1024 then s"${bytes}B"
    else if bytes < 1024 * 1024 then f"${bytes / 1024.0}%.1fKB"
    else f"${bytes / (1024.0 * 1024.0)}%.1fMB"

  /** Format a skill label with name, subpath, and size for display in selection lists. */
  def skillLabel(skillName: String, subpath: String, size: Long): String =
    s"${skillName.padTo(25, ' ')} ($subpath)".padTo(60, ' ') + s" ${formatSize(size)}"

  /** Format a skill name with its subpath for display in messages. */
  def skillNameWithSubpath(skillName: String, subpath: String): String =
    s"$skillName ($subpath)"

  /** Read the subpath of an already-installed skill from its metadata. */
  def existingSubpathLabel(targetPath: os.Path): String =
    SkillMetadata.readSkillMetadata(targetPath).flatMap(_.subpath).getOrElse("")

  /** Select items from a list by matching their labels against selected labels. */
  def selectByLabel[A](labels: List[String], items: List[A], selectedLabels: List[String]): List[A] =
    selectedLabels
      .flatMap { label =>
        labels.zipWithIndex.find { case (l, _) => l === label }.map { case (_, idx) => idx }
      }
      .map(items(_))

  /** Resolve agents and location from options, prompting interactively when not specified. */
  private def resolveAgentsAndLocation(options: InstallOptions): (List[Agent], Set[SkillLocation]) = {
    options.agent match {
      case Some(agents) => (agents, options.locations)
      case None =>
        val agents    = promptForAgents()
        val locations = if options.locations.nonEmpty then options.locations else promptForLocation(agents)
        (agents, locations)
    }
  }

  private def promptForAgents(): List[Agent] = {
    val agentLabels = Agent.all.map(_.toString)

    aiskills.cli.SigintHandler.install()
    val result = Prompts.sync.use { prompts =>
      prompts.run(CliDefaults.mandatoryMultiChoiceNoneSelected("Select target agent(s)", agentLabels)) match {
        case Completion.Finished(selectedLabels) =>
          Agent.all.filter(a => selectedLabels.contains(a.toString)).asRight
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
    result match {
      case Left(code) => throw SkillInstallException(code) // scalafix:ok DisableSyntax.throw
      case Right(agents) => agents
    }
  }

  private def promptForLocation(agents: List[Agent]): Set[SkillLocation] = {
    val projectPaths   = agents.map(_.projectDirName).distinct.mkString(", ")
    val globalPaths    = agents.map(a => s"~/${a.globalDirName}").distinct.mkString(", ")
    val locationLabels = List(
      s"global  ($globalPaths)",
      s"project ($projectPaths)",
      "both",
    )

    aiskills.cli.SigintHandler.install()
    val result = Prompts.sync.use { prompts =>
      prompts.singleChoice("Select install location", locationLabels) match {
        case Completion.Finished(selected) =>
          if selected.startsWith("project") then Set(SkillLocation.Project).asRight
          else if selected.startsWith("global") then Set(SkillLocation.Global).asRight
          else Set(SkillLocation.Global, SkillLocation.Project).asRight
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
    result match {
      case Left(code) => throw SkillInstallException(code) // scalafix:ok DisableSyntax.throw
      case Right(locations) => locations
    }
  }

  /** Install skill from local path, GitHub, or Git URL. */
  def installSkill(source: String, options: InstallOptions): Unit = {
    aiskills.cli.TempDirCleanup.ensureAtexitRegistered()

    // Resolve agents and location (interactive if not specified) before cloning
    val (agents, locations) = resolveAgentsAndLocation(options)

    // Resolve source once
    val resolvedSource = resolveSource(source)

    try {
      for {
        agent    <- agents
        location <- locations
      } do {
        val isProject    = location === SkillLocation.Project
        val folder       = s"${agent.projectDirName}/skills"
        val globalFolder = s"${agent.globalDirName}/skills"
        val targetDir    =
          if isProject then os.pwd / os.RelPath(folder)
          else os.home / os.RelPath(globalFolder)

        val locationDisplay =
          if isProject then s"project ($folder)".blue
          else s"global (~/$globalFolder)".dim

        if agents.length > 1 || locations.size > 1
        then println(s"\n--- ${agent.toString} (${location.toString.toLowerCase}) ---".bold)
        else ()

        println(s"Installing from: ${source.cyan}")
        println(s"Location: $locationDisplay")
        if agents.length <= 1 && locations.size <= 1 then {
          if !isProject then println(s"Global install selected (~/$globalFolder). Omit --global for ./$folder.".dim)
          else ()
        } else ()
        println()

        resolvedSource match {
          case ResolvedSource.Local(localPath, sourceInfo) =>
            installFromLocal(localPath, targetDir, options, sourceInfo)

          case ResolvedSource.Git(repoDir, repoUrl, skillSubpath, sourceInfo) =>
            if skillSubpath.nonEmpty then installSpecificSkill(
              repoDir,
              skillSubpath,
              targetDir,
              isProject,
              options,
              sourceInfo
            )
            else {
              val repoName = getRepoName(repoUrl)
              installFromRepo(repoDir, targetDir, options, repoName, sourceInfo)
            }
        }

        AgentsMd.updateAgentsMdForAgent(agent, location)
      }

      printPostInstallHints(locations)
    } finally {
      resolvedSource.cleanup()
    }
  }

  final class SkillInstallException(val exitCode: Int) extends RuntimeException

  /** Resolved source — either local or a cloned git repo. */
  private enum ResolvedSource {
    case Local(localPath: os.Path, sourceInfo: InstallSourceInfo)
    case Git(repoDir: os.Path, repoUrl: String, skillSubpath: String, sourceInfo: InstallSourceInfo)
  }

  private object ResolvedSource {
    extension (rs: ResolvedSource) {
      def cleanup(): Unit = rs match {
        case ResolvedSource.Git(repoDir, _, _, _) =>
          val tempDir = repoDir / os.up
          aiskills.cli.TempDirCleanup.safeRemoveAll(tempDir)
          aiskills.cli.TempDirCleanup.unregister()
        case _ => ()
      }
    }
  }

  /** Parse a non-local source into (repoUrl, skillSubpath). */
  private def parseGitSource(source: String): (String, String) = {
    if isGitUrl(source) then (source, "")
    else {
      val parts = source.split("/").toList
      parts match {
        case owner :: repo :: Nil =>
          (s"https://github.com/$source", "")
        case owner :: repo :: rest =>
          (s"https://github.com/$owner/$repo", rest.mkString("/"))
        case _ =>
          System.err.println("Error: Invalid source format".red)
          System.err.println("Expected: owner/repo, owner/repo/skill-name, git URL, or local path")
          throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
      }
    }
  }

  /** Resolve source into either a local path or a cloned git repo. */
  private def resolveSource(source: String): ResolvedSource = {
    if isLocalPath(source) then {
      val localPath  = expandPath(source)
      val sourceInfo = InstallSourceInfo(
        source = source,
        sourceType = SkillSourceType.Local,
        repoUrl = none[String],
        localRoot = localPath.some,
      )
      ResolvedSource.Local(localPath, sourceInfo)
    } else {
      val (repoUrl, skillSubpath) = parseGitSource(source)

      val tempDir = os.home / s".aiskills-temp-${System.currentTimeMillis()}"
      os.makeDir.all(tempDir)
      aiskills.cli.TempDirCleanup.register(tempDir)

      val spinner = Spinner.createDefaultSideEffect(
        SpinnerConfig
          .default
          .withText("Cloning repository...")
          .withColor(Color.cyan)
          .withIndent(2),
      )
      val _       = spinner.start()

      val actualUrl =
        Try {
          cloneWithFallback(repoUrl, (tempDir / "repo").toString)
        } match {
          case Failure(ex) =>
            val _   = spinner.fail(Some("Clone failed"))
            val msg = ex.getMessage
            if msg.nonEmpty then println(msg.dim) else ()
            println("\nTip: For private repos, ensure git SSH keys or credentials are configured".yellow)
            aiskills.cli.TempDirCleanup.safeRemoveAll(tempDir)
            aiskills.cli.TempDirCleanup.unregister()
            throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
          case Success(url) =>
            val _ = spinner.succeed(Some("Repository cloned"))
            url
        }

      val sourceInfo = InstallSourceInfo(
        source = source,
        sourceType = SkillSourceType.Git,
        repoUrl = actualUrl.some,
        localRoot = none[os.Path],
      )

      ResolvedSource.Git(tempDir / "repo", actualUrl, skillSubpath, sourceInfo)
    }
  }

  private def printPostInstallHints(locations: Set[SkillLocation]): Unit = {
    println(s"\n${"Read skill:".dim} ${"aiskills read <skill-name>".cyan}")
    if locations.contains(SkillLocation.Project) then println(
      s"${"Sync to other agents:".dim} ${"aiskills sync <skill-name> --from <agent> --to <agent>".cyan}"
    )
    else ()
  }

  private def installFromLocal(
    localPath: os.Path,
    targetDir: os.Path,
    options: InstallOptions,
    sourceInfo: InstallSourceInfo,
  ): Unit = {
    if !os.exists(localPath) then {
      System.err.println(s"Error: Path does not exist: $localPath".red)
      throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
    } else if !os.isDir(localPath) then {
      System.err.println("Error: Path must be a directory".red)
      throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
    } else {
      val skillMdPath = localPath / "SKILL.md"
      if os.exists(skillMdPath) then {
        val isProject = targetDir.startsWith(os.pwd)
        installSingleLocalSkill(localPath, targetDir, isProject, options, sourceInfo)
      } else
        installFromRepo(localPath, targetDir, options, none[String], sourceInfo)
    }
  }

  private def installSingleLocalSkill(
    skillDir: os.Path,
    targetDir: os.Path,
    isProject: Boolean,
    options: InstallOptions,
    sourceInfo: InstallSourceInfo,
  ): Unit = {
    val skillMdPath = skillDir / "SKILL.md"
    val content     = os.read(skillMdPath)

    if !Yaml.hasValidFrontmatter(content) then {
      System.err.println("Error: Invalid SKILL.md (missing YAML frontmatter)".red)
      throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
    } else {
      val skillName  = skillDir.last
      val targetPath = targetDir / skillName

      resolveConflict(skillName, targetPath, targetDir, isProject, options.yes) match {
        case ConflictResolution.Skip =>
          println(s"Skipped: $skillName".yellow)

        case ConflictResolution.Overwrite | ConflictResolution.NoConflict =>
          os.makeDir.all(targetDir)
          if !isPathInside(targetPath, targetDir) then {
            System.err.println("Security error: Installation path outside target directory".red)
            throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
          } else {
            os.copy(skillDir, targetPath, replaceExisting = true)
            SkillMetadata.writeSkillMetadata(targetPath, buildLocalMetadata(sourceInfo, skillDir))

            println(s"\u2705 Installed: $skillName".green)
            println(s"   Location: $targetPath")
          }

        case ConflictResolution.Rename(newName) =>
          installSkillWithRename(skillDir, targetDir, newName, buildLocalMetadata(sourceInfo, skillDir))
          println(s"\u2705 Installed: $skillName as $newName".green)
          println(s"   Location: ${targetDir / newName}")
      }
    }
  }

  private def installSpecificSkill(
    repoDir: os.Path,
    skillSubpath: String,
    targetDir: os.Path,
    isProject: Boolean,
    options: InstallOptions,
    sourceInfo: InstallSourceInfo,
  ): Unit = {
    val skillDir    = repoDir / os.RelPath(skillSubpath)
    val skillMdPath = skillDir / "SKILL.md"

    if !os.exists(skillMdPath) then {
      System.err.println(s"Error: SKILL.md not found at $skillSubpath".red)
      throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
    } else {
      val content = os.read(skillMdPath)
      if !Yaml.hasValidFrontmatter(content) then {
        System.err.println("Error: Invalid SKILL.md (missing YAML frontmatter)".red)
        throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
      } else {
        val skillName  = skillSubpath.split("/").last
        val targetPath = targetDir / skillName

        resolveConflict(skillName, targetPath, targetDir, isProject, options.yes) match {
          case ConflictResolution.Skip =>
            println(s"Skipped: $skillName".yellow)

          case ConflictResolution.Overwrite | ConflictResolution.NoConflict =>
            os.makeDir.all(targetDir)
            if !isPathInside(targetPath, targetDir) then {
              System.err.println("Security error: Installation path outside target directory".red)
              throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
            } else {
              os.copy(skillDir, targetPath, replaceExisting = true)
              SkillMetadata.writeSkillMetadata(targetPath, buildGitMetadata(sourceInfo, skillSubpath))

              println(s"\u2705 Installed: $skillName".green)
              println(s"   Location: $targetPath")
            }

          case ConflictResolution.Rename(newName) =>
            installSkillWithRename(skillDir, targetDir, newName, buildGitMetadata(sourceInfo, skillSubpath))
            println(s"\u2705 Installed: $skillName as $newName".green)
            println(s"   Location: ${targetDir / newName}")
        }
      }
    }
  }

  private def installFromRepo(
    repoDir: os.Path,
    targetDir: os.Path,
    options: InstallOptions,
    repoName: Option[String],
    sourceInfo: InstallSourceInfo,
  ): Unit = {
    val rootSkillPath = repoDir / "SKILL.md"

    final case class SkillInfo(
      skillDir: os.Path,
      skillName: String,
      subpath: String,
      description: String,
      targetPath: os.Path,
      size: Long,
    )

    val rootSkillInfos: List[SkillInfo] =
      if os.exists(rootSkillPath) then {
        val content = os.read(rootSkillPath)
        if !Yaml.hasValidFrontmatter(content) then {
          System.err.println("Error: Invalid SKILL.md (missing YAML frontmatter)".red)
          throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
        } else {
          val frontmatterName = Yaml.extractYamlField(content, "name")
          val skillName       =
            if frontmatterName.nonEmpty then frontmatterName
            else repoName.getOrElse(repoDir.last)
          List(
            SkillInfo(
              skillDir = repoDir,
              skillName = skillName,
              subpath = "",
              description = Yaml.extractYamlField(content, "description"),
              targetPath = targetDir / skillName,
              size = getDirectorySize(repoDir),
            )
          )
        }
      } else
        Nil

    val skillInfos: List[SkillInfo] =
      if rootSkillInfos.nonEmpty then rootSkillInfos
      else {
        def findSkills(dir: os.Path): List[os.Path] =
          os.list(dir).toList.flatMap { entry =>
            if os.isDir(entry) then {
              if os.exists(entry / "SKILL.md") then List(entry)
              else findSkills(entry)
            } else Nil
          }

        val skillDirs = findSkills(repoDir)

        if skillDirs.isEmpty then {
          System.err.println("Error: No SKILL.md files found in repository".red)
          throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
        } else {
          val fromDirs = skillDirs.flatMap { skillDir =>
            val skillMdPath = skillDir / "SKILL.md"
            val content     = os.read(skillMdPath)

            if !Yaml.hasValidFrontmatter(content) then none[SkillInfo]
            else {
              val skillName   = skillDir.last
              val subpath     = skillDir.relativeTo(repoDir).toString
              val description = Yaml.extractYamlField(content, "description")
              val targetPath  = targetDir / skillName
              val size        = getDirectorySize(skillDir)
              SkillInfo(skillDir, skillName, subpath, description, targetPath, size).some
            }
          }

          if fromDirs.isEmpty then {
            System.err.println("Error: No valid SKILL.md files found".red)
            throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
          } else
            fromDirs
        }
      }

    println(s"Found ${skillInfos.length} skill(s)\n".dim)

    val skillsToInstall: List[SkillInfo] =
      if !options.yes && skillInfos.length > 1 then {
        val labels = skillInfos.map { info =>
          skillLabel(info.skillName, info.subpath, info.size)
        }

        aiskills.cli.SigintHandler.install()
        val result = Prompts.sync.use { prompts =>
          prompts.run(CliDefaults.mandatoryMultiChoiceAllSelected("Select skills to install", labels)) match {
            case Completion.Finished(selectedLabels) =>
              selectByLabel(labels, skillInfos, selectedLabels).asRight

            case Completion.Fail(CompletionError.Interrupted) =>
              println("\n\nCancelled by user".yellow)
              0.asLeft

            case Completion.Fail(CompletionError.Error(msg)) =>
              System.err.println(s"Error: $msg")
              1.asLeft
          }
        }
        result match {
          case Left(code) => throw SkillInstallException(code) // scalafix:ok DisableSyntax.throw
          case Right(list) => list
        }
      } else
        skillInfos

    if skillsToInstall.nonEmpty then {
      val isProject = targetDir.startsWith(os.pwd)

      val (installedCount, _) =
        skillsToInstall.foldLeft((0, BulkDecision.Undecided: BulkDecision)) {
          case ((count, bulk), info) =>
            def doInstall(): Int = {
              if os.exists(info.targetPath) then {
                val existingSubpath = existingSubpathLabel(info.targetPath)
                println(
                  s"Replacing: ${skillNameWithSubpath(info.skillName, existingSubpath)} -> ${skillNameWithSubpath(info.skillName, info.subpath)}".dim
                )
                os.remove.all(info.targetPath)
              } else ()
              os.makeDir.all(targetDir)
              if !isPathInside(info.targetPath, targetDir) then {
                System.err.println("Security error: Installation path outside target directory".red)
                count
              } else {
                os.copy(info.skillDir, info.targetPath, replaceExisting = true)
                SkillMetadata.writeSkillMetadata(
                  info.targetPath,
                  buildMetadataFromSource(sourceInfo, info.skillDir, repoDir),
                )
                println(s"\u2705 Installed: ${skillNameWithSubpath(info.skillName, info.subpath)}".green)
                count + 1
              }
            }

            if !os.exists(info.targetPath) then {
              // No conflict — check marketplace warning and install
              if !isProject && MarketplaceSkills.anthropicMarketplaceSkills.contains(info.skillName) then {
                System
                  .err
                  .println(
                    s"\n\u26a0\ufe0f  Warning: '${info.skillName}' matches an Anthropic marketplace skill".yellow
                  )
                System.err.println("   Installing globally may conflict with Claude Code plugins.".dim)
                System.err.println("   If you re-enable Claude plugins, this will be overwritten.".dim)
                System.err.println("   Recommend: Use --project flag for conflict-free installation.\n".dim)
              } else ()
              os.makeDir.all(targetDir)
              if !isPathInside(info.targetPath, targetDir) then {
                System.err.println("Security error: Installation path outside target directory".red)
                (count, bulk)
              } else {
                os.copy(info.skillDir, info.targetPath, replaceExisting = true)
                SkillMetadata.writeSkillMetadata(
                  info.targetPath,
                  buildMetadataFromSource(sourceInfo, info.skillDir, repoDir),
                )
                println(s"\u2705 Installed: ${skillNameWithSubpath(info.skillName, info.subpath)}".green)
                (count + 1, bulk)
              }
            } else if options.yes then (doInstall(), bulk)
            else
              bulk match {
                case BulkDecision.OverwriteAll =>
                  (doInstall(), bulk)

                case BulkDecision.SkipAll =>
                  println(s"Skipped: ${skillNameWithSubpath(info.skillName, info.subpath)}".yellow)
                  (count, bulk)

                case BulkDecision.Undecided =>
                  val existingSubpath = existingSubpathLabel(info.targetPath)
                  println(s"   Existing: ${skillNameWithSubpath(info.skillName, existingSubpath)}".dim)
                  println(s"   New:      ${skillNameWithSubpath(info.skillName, info.subpath)}".dim)
                  OverwritePrompt.askOverwriteChoice(
                    info.skillName,
                    s"Skill '${info.skillName}' already exists. Replace with '${skillNameWithSubpath(info.skillName, info.subpath)}'?",
                  ) match {
                    case Left(code) =>
                      throw SkillInstallException(code) // scalafix:ok DisableSyntax.throw

                    case Right(OverwriteChoice.Yes) =>
                      (doInstall(), BulkDecision.Undecided)

                    case Right(OverwriteChoice.No) =>
                      println(s"Skipped: ${skillNameWithSubpath(info.skillName, info.subpath)}".yellow)
                      (count, BulkDecision.Undecided)

                    case Right(OverwriteChoice.Rename) =>
                      OverwritePrompt.askNewSkillName(info.skillName, targetDir) match {
                        case Left(code) =>
                          throw SkillInstallException(code) // scalafix:ok DisableSyntax.throw
                        case Right(newName) =>
                          installSkillWithRename(
                            info.skillDir,
                            targetDir,
                            newName,
                            buildMetadataFromSource(sourceInfo, info.skillDir, repoDir),
                          )
                          println(
                            s"\u2705 Installed: ${skillNameWithSubpath(info.skillName, info.subpath)} as $newName".green
                          )
                          (count + 1, BulkDecision.Undecided)
                      }

                    case Right(OverwriteChoice.YesToAll) =>
                      (doInstall(), BulkDecision.OverwriteAll)

                    case Right(OverwriteChoice.NoToAll) =>
                      println(s"Skipped: ${skillNameWithSubpath(info.skillName, info.subpath)}".yellow)
                      (count, BulkDecision.SkipAll)
                  }
              }
        }

      println(s"\n\u2705 Installation complete: $installedCount skill(s) installed".green)
    } else ()
  }

  private def buildMetadataFromSource(
    sourceInfo: InstallSourceInfo,
    skillDir: os.Path,
    repoDir: os.Path,
  ): SkillSourceMetadata =
    if sourceInfo.sourceType === SkillSourceType.Local then buildLocalMetadata(sourceInfo, skillDir)
    else {
      val subpath           = skillDir.relativeTo(repoDir).toString
      val normalizedSubpath = if subpath === "." then "" else subpath
      buildGitMetadata(sourceInfo, normalizedSubpath)
    }

  private def buildGitMetadata(sourceInfo: InstallSourceInfo, subpath: String): SkillSourceMetadata =
    SkillSourceMetadata(
      source = sourceInfo.source,
      sourceType = SkillSourceType.Git,
      repoUrl = sourceInfo.repoUrl,
      subpath = subpath.some,
      localPath = none[String],
      installedAt = aiskills.core.utils.isoNow(),
    )

  private def buildLocalMetadata(sourceInfo: InstallSourceInfo, skillDir: os.Path): SkillSourceMetadata =
    SkillSourceMetadata(
      source = sourceInfo.source,
      sourceType = SkillSourceType.Local,
      repoUrl = none[String],
      subpath = none[String],
      localPath = skillDir.toString.some,
      installedAt = aiskills.core.utils.isoNow(),
    )

  private enum ConflictResolution {
    case Overwrite
    case Skip
    case Rename(newName: String)
    case NoConflict
  }

  private def resolveConflict(
    skillName: String,
    targetPath: os.Path,
    targetDir: os.Path,
    isProject: Boolean,
    skipPrompt: Boolean,
  ): ConflictResolution = {
    if os.exists(targetPath) then {
      if skipPrompt then {
        println(s"Overwriting: $skillName (all existing files and folders will be removed)".dim)
        os.remove.all(targetPath)
        ConflictResolution.Overwrite
      } else {
        val options = List(
          "Overwrite    — Replace the existing skill",
          "Skip         — Keep the existing skill",
          s"${"Rename".yellow}       — Keep both (install under a new name)",
        )
        aiskills.cli.SigintHandler.install()
        val result  = Prompts.sync.use { prompts =>
          println(
            s"\u26a0 All existing files and folders in '$skillName' will be removed if you choose to overwrite.".yellow
          )
          prompts.singleChoice(s"Skill '$skillName' already exists. What would you like to do?".yellow, options) match {
            case Completion.Finished(selected) =>
              if selected.startsWith("Overwrite") then {
                os.remove.all(targetPath)
                ConflictResolution.Overwrite.asRight
              } else if selected.contains("Rename") then {
                OverwritePrompt.askNewSkillName(skillName, targetDir) match {
                  case Left(code) => code.asLeft
                  case Right(newName) => ConflictResolution.Rename(newName).asRight
                }
              } else ConflictResolution.Skip.asRight
            case Completion.Fail(CompletionError.Interrupted) =>
              println("\n\nCancelled by user".yellow)
              0.asLeft
            case Completion.Fail(CompletionError.Error(_)) =>
              ConflictResolution.Skip.asRight
          }
        }
        result match {
          case Left(code) => throw SkillInstallException(code) // scalafix:ok DisableSyntax.throw
          case Right(v) => v
        }
      }
    } else {
      if !isProject && MarketplaceSkills.anthropicMarketplaceSkills.contains(skillName) then {
        System.err.println(s"\n\u26a0\ufe0f  Warning: '$skillName' matches an Anthropic marketplace skill".yellow)
        System.err.println("   Installing globally may conflict with Claude Code plugins.".dim)
        System.err.println("   If you re-enable Claude plugins, this will be overwritten.".dim)
        System.err.println("   Recommend: Use --project flag for conflict-free installation.\n".dim)
      } else ()
      ConflictResolution.NoConflict
    }
  }

  /** Install a skill under a new name (rename flow). */
  private def installSkillWithRename(
    sourceDir: os.Path,
    targetDir: os.Path,
    newName: String,
    metadata: SkillSourceMetadata,
  ): Unit = {
    val targetPath = targetDir / newName
    os.makeDir.all(targetDir)
    if !isPathInside(targetPath, targetDir) then {
      System.err.println("Security error: Installation path outside target directory".red)
      throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
    } else {
      os.copy(sourceDir, targetPath, replaceExisting = true)
      val skillMdPath = targetPath / "SKILL.md"
      if os.exists(skillMdPath) then {
        val content = os.read(skillMdPath)
        val updated = Yaml.replaceYamlField(content, "name", newName)
        os.write.over(skillMdPath, updated)
      } else ()
      SkillMetadata.writeSkillMetadata(targetPath, metadata.copy(name = newName.some))
    }
  }
}
