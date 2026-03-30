package aiskills.cli.commands

import aiskills.core.{Agent, InstallOptions, InstallSourceInfo, SkillLocation, SkillSourceMetadata, SkillSourceType}
import aiskills.core.utils.{AgentsMd, MarketplaceSkills, SkillMetadata, Yaml}
import cats.syntax.all.*
import extras.scala.io.syntax.color.*
import cue4s.*

import OverwritePrompt.{BulkDecision, OverwriteChoice}

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
      prompts.multiChoiceNoneSelected("Select target agent(s)", agentLabels) match {
        case Completion.Finished(selectedLabels) =>
          val selected = Agent.all.filter(a => selectedLabels.contains(a.toString))
          if selected.isEmpty then {
            println("No agents selected. Installation cancelled.".yellow)
            Left(0)
          } else Right(selected)
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          Left(0)
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          Left(1)
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
      s"project ($projectPaths)",
      s"global  ($globalPaths)",
    )

    aiskills.cli.SigintHandler.install()
    val result = Prompts.sync.use { prompts =>
      prompts.multiChoiceNoneSelected("Select install location", locationLabels) match {
        case Completion.Finished(selectedLabels) =>
          if selectedLabels.isEmpty then {
            println("No location selected. Defaulting to project.".yellow)
            Right(Set(SkillLocation.Project))
          } else {
            val locs = selectedLabels.foldLeft(Set.empty[SkillLocation]) { (acc, label) =>
              if label.startsWith("project") then acc + SkillLocation.Project
              else acc + SkillLocation.Global
            }
            Right(locs)
          }
        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          Left(0)
        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          Left(1)
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

      val tempDir    = os.home / s".aiskills-temp-${System.currentTimeMillis()}"
      os.makeDir.all(tempDir)
      aiskills.cli.TempDirCleanup.register(tempDir)
      val sourceInfo = InstallSourceInfo(
        source = source,
        sourceType = SkillSourceType.Git,
        repoUrl = repoUrl.some,
        localRoot = none[os.Path],
      )

      val spinner = Spinner.createDefaultSideEffect(
        SpinnerConfig
          .default
          .withText("Cloning repository...")
          .withColor(Color.cyan)
          .withIndent(2),
      )
      val _       = spinner.start()
      Try {
        os.proc("git", "clone", "--depth", "1", "--quiet", repoUrl, (tempDir / "repo").toString)
          .call(stderr = os.Pipe)
      } match {
        case Failure(ex) =>
          val _   = spinner.fail(Some("Clone failed"))
          val msg = ex.getMessage
          if msg.nonEmpty then println(msg.dim) else ()
          println("\nTip: For private repos, ensure git SSH keys or credentials are configured".yellow)
          aiskills.cli.TempDirCleanup.safeRemoveAll(tempDir)
          aiskills.cli.TempDirCleanup.unregister()
          throw SkillInstallException(1) // scalafix:ok DisableSyntax.throw
        case Success(_) =>
          val _ = spinner.succeed(Some("Repository cloned"))
      }

      ResolvedSource.Git(tempDir / "repo", repoUrl, skillSubpath, sourceInfo)
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

      if !warnIfConflict(skillName, targetPath, isProject, options.yes) then println(s"Skipped: $skillName".yellow)
      else {
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

        if !warnIfConflict(skillName, targetPath, isProject, options.yes) then println(s"Skipped: $skillName".yellow)
        else {
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
              val description = Yaml.extractYamlField(content, "description")
              val targetPath  = targetDir / skillName
              val size        = getDirectorySize(skillDir)
              SkillInfo(skillDir, skillName, description, targetPath, size).some
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
          s"${info.skillName.padTo(25, ' ')} ${formatSize(info.size)}"
        }

        aiskills.cli.SigintHandler.install()
        val result = Prompts.sync.use { prompts =>
          prompts.multiChoiceAllSelected("Select skills to install", labels) match {
            case Completion.Finished(selectedLabels) =>
              if selectedLabels.isEmpty then {
                println("No skills selected. Installation cancelled.".yellow)
                Right(Nil)
              } else
                Right(skillInfos.filter { info =>
                  selectedLabels.exists(_.contains(info.skillName))
                })

            case Completion.Fail(CompletionError.Interrupted) =>
              println("\n\nCancelled by user".yellow)
              Left(0)

            case Completion.Fail(CompletionError.Error(msg)) =>
              System.err.println(s"Error: $msg")
              Left(1)
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
                println(s"Overwriting: ${info.skillName} (all existing files and folders will be removed)".dim)
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
                println(s"\u2705 Installed: ${info.skillName}".green)
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
                println(s"\u2705 Installed: ${info.skillName}".green)
                (count + 1, bulk)
              }
            } else if options.yes then (doInstall(), bulk)
            else
              bulk match {
                case BulkDecision.OverwriteAll =>
                  (doInstall(), bulk)

                case BulkDecision.SkipAll =>
                  println(s"Skipped: ${info.skillName}".yellow)
                  (count, bulk)

                case BulkDecision.Undecided =>
                  OverwritePrompt.askOverwriteChoice(
                    info.skillName,
                    s"Skill '${info.skillName}' already exists. What would you like to do?",
                  ) match {
                    case Left(code) =>
                      throw SkillInstallException(code) // scalafix:ok DisableSyntax.throw

                    case Right(OverwriteChoice.Yes) =>
                      (doInstall(), BulkDecision.Undecided)

                    case Right(OverwriteChoice.No) =>
                      println(s"Skipped: ${info.skillName}".yellow)
                      (count, BulkDecision.Undecided)

                    case Right(OverwriteChoice.YesToAll) =>
                      (doInstall(), BulkDecision.OverwriteAll)

                    case Right(OverwriteChoice.NoToAll) =>
                      println(s"Skipped: ${info.skillName}".yellow)
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

  private def warnIfConflict(
    skillName: String,
    targetPath: os.Path,
    isProject: Boolean,
    skipPrompt: Boolean,
  ): Boolean = {
    if os.exists(targetPath) then {
      if skipPrompt then {
        println(s"Overwriting: $skillName (all existing files and folders will be removed)".dim)
        os.remove.all(targetPath)
        true
      } else {
        aiskills.cli.SigintHandler.install()
        val result = Prompts.sync.use { prompts =>
          println(
            s"\u26a0 All existing files and folders in '$skillName' will be removed if you choose to overwrite.".yellow
          )
          prompts.confirm(s"Skill '$skillName' already exists. Overwrite?".yellow, default = false) match {
            case Completion.Finished(shouldOverwrite) =>
              if shouldOverwrite then os.remove.all(targetPath) else ()
              Right(shouldOverwrite)
            case Completion.Fail(CompletionError.Interrupted) =>
              println("\n\nCancelled by user".yellow)
              Left(0)
            case Completion.Fail(CompletionError.Error(_)) =>
              Right(false)
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
      true
    }
  }
}
