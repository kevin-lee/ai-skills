package aiskills.cli.commands

import aiskills.core.{Agent, InstallOptions, InstallSourceInfo, SkillSourceMetadata, SkillSourceType}
import aiskills.core.utils.{AgentsMd, MarketplaceSkills, SkillMetadata, Yaml}
import extras.scala.io.syntax.color.*
import cue4s.*

import scala.util.{Try, Success, Failure}

object Install:

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
  def getRepoName(repoUrl: String): Option[String] =
    val cleaned = repoUrl.stripSuffix(".git")
    cleaned.split("/").lastOption.flatMap { lastPart =>
      val maybeRepo = if lastPart.contains(":") then lastPart.split(":").lastOption else Some(lastPart)
      maybeRepo.filter(_.nonEmpty)
    }

  /** Expand ~ to home directory. */
  def expandPath(source: String): os.Path =
    if source.startsWith("~/") || source.startsWith("$HOME/") then
      os.home / os.RelPath(source.drop(2))
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

  /** Install skill from local path, GitHub, or Git URL. */
  def installSkill(source: String, options: InstallOptions): Unit =
    val agents = if options.allAgents then Agent.all else List(options.agent)
    val isProject = !options.global

    // Resolve source once
    val resolvedSource = resolveSource(source)

    for agent <- agents do
      val folder = s"${agent.projectDirName}/skills"
      val globalFolder = s"${agent.globalDirName}/skills"
      val targetDir =
        if isProject then os.pwd / os.RelPath(folder)
        else os.home / os.RelPath(globalFolder)

      val location =
        if isProject then s"project ($folder)".blue
        else s"global (~/$globalFolder)".dim

      if agents.length > 1 then
        println(s"\n--- ${agent.toString} ---".bold)

      println(s"Installing from: ${source.cyan}")
      println(s"Location: $location")
      if agents.length <= 1 then
        if isProject then
          println(s"Default install is project-local (./$folder). Use --global for ~/$globalFolder.".dim)
        else
          println(s"Global install selected (~/$globalFolder). Omit --global for ./$folder.".dim)
      println()

      resolvedSource match
        case ResolvedSource.Local(localPath, sourceInfo) =>
          installFromLocal(localPath, targetDir, options, sourceInfo)

        case ResolvedSource.Git(repoDir, repoUrl, skillSubpath, sourceInfo) =>
          if skillSubpath.nonEmpty then
            installSpecificSkill(repoDir, skillSubpath, targetDir, isProject, options, sourceInfo)
          else
            val repoName = getRepoName(repoUrl)
            installFromRepo(repoDir, targetDir, options, repoName, sourceInfo)

      AgentsMd.updateAgentsMdForAgent(agent, options.global)

    resolvedSource match
      case ResolvedSource.Git(_, _, _, _) => resolvedSource.cleanup()
      case _                              => ()

    printPostInstallHints(isProject)

  /** Resolved source — either local or a cloned git repo. */
  private enum ResolvedSource:
    case Local(localPath: os.Path, sourceInfo: InstallSourceInfo)
    case Git(repoDir: os.Path, repoUrl: String, skillSubpath: String, sourceInfo: InstallSourceInfo)

    def cleanup(): Unit = this match
      case Git(repoDir, _, _, _) =>
        val tempDir = repoDir / os.up
        os.remove.all(tempDir)
      case _ => ()

  /** Resolve source into either a local path or a cloned git repo. */
  private def resolveSource(source: String): ResolvedSource =
    if isLocalPath(source) then
      val localPath = expandPath(source)
      val sourceInfo = InstallSourceInfo(
        source = source,
        sourceType = SkillSourceType.Local,
        localRoot = Some(localPath),
      )
      ResolvedSource.Local(localPath, sourceInfo)
    else
      var repoUrl: String = ""
      var skillSubpath: String = ""

      if isGitUrl(source) then
        repoUrl = source
      else
        val parts = source.split("/").toList
        parts match
          case owner :: repo :: Nil =>
            repoUrl = s"https://github.com/$source"
          case owner :: repo :: rest =>
            repoUrl = s"https://github.com/$owner/$repo"
            skillSubpath = rest.mkString("/")
          case _ =>
            System.err.println("Error: Invalid source format".red)
            System.err.println("Expected: owner/repo, owner/repo/skill-name, git URL, or local path")
            sys.exit(1)

      val tempDir = os.home / s".aiskills-temp-${System.currentTimeMillis()}"
      os.makeDir.all(tempDir)
      val sourceInfo = InstallSourceInfo(
        source = source,
        sourceType = SkillSourceType.Git,
        repoUrl = Some(repoUrl),
      )

      print("Cloning repository...")
      Try {
        os.proc("git", "clone", "--depth", "1", "--quiet", repoUrl, (tempDir / "repo").toString)
          .call(stderr = os.Pipe)
      } match
        case Failure(ex) =>
          println(" failed")
          val msg = ex.getMessage
          if msg.nonEmpty then println(msg.dim)
          println("\nTip: For private repos, ensure git SSH keys or credentials are configured".yellow)
          os.remove.all(tempDir)
          sys.exit(1)
        case Success(_) =>
          println(" done")

      ResolvedSource.Git(tempDir / "repo", repoUrl, skillSubpath, sourceInfo)

  private def printPostInstallHints(isProject: Boolean): Unit =
    println(s"\n${"Read skill:".dim} ${"aiskills read <skill-name>".cyan}")
    if isProject then
      println(s"${"Sync to other agents:".dim} ${"aiskills sync <skill-name> --from <agent> --to <agent>".cyan}")

  private def installFromLocal(
    localPath: os.Path,
    targetDir: os.Path,
    options: InstallOptions,
    sourceInfo: InstallSourceInfo,
  ): Unit =
    if !os.exists(localPath) then
      System.err.println(s"Error: Path does not exist: $localPath".red)
      sys.exit(1)

    if !os.isDir(localPath) then
      System.err.println("Error: Path must be a directory".red)
      sys.exit(1)

    val skillMdPath = localPath / "SKILL.md"
    if os.exists(skillMdPath) then
      val isProject = targetDir.startsWith(os.pwd)
      installSingleLocalSkill(localPath, targetDir, isProject, options, sourceInfo)
    else
      installFromRepo(localPath, targetDir, options, None, sourceInfo)

  private def installSingleLocalSkill(
    skillDir: os.Path,
    targetDir: os.Path,
    isProject: Boolean,
    options: InstallOptions,
    sourceInfo: InstallSourceInfo,
  ): Unit =
    val skillMdPath = skillDir / "SKILL.md"
    val content = os.read(skillMdPath)

    if !Yaml.hasValidFrontmatter(content) then
      System.err.println("Error: Invalid SKILL.md (missing YAML frontmatter)".red)
      sys.exit(1)

    val skillName = skillDir.last
    val targetPath = targetDir / skillName

    if !warnIfConflict(skillName, targetPath, isProject, options.yes) then
      println(s"Skipped: $skillName".yellow)
      return

    os.makeDir.all(targetDir)
    if !isPathInside(targetPath, targetDir) then
      System.err.println("Security error: Installation path outside target directory".red)
      sys.exit(1)

    os.copy(skillDir, targetPath, replaceExisting = true)
    SkillMetadata.writeSkillMetadata(targetPath, buildLocalMetadata(sourceInfo, skillDir))

    println(s"\u2705 Installed: $skillName".green)
    println(s"   Location: $targetPath")

  private def installSpecificSkill(
    repoDir: os.Path,
    skillSubpath: String,
    targetDir: os.Path,
    isProject: Boolean,
    options: InstallOptions,
    sourceInfo: InstallSourceInfo,
  ): Unit =
    val skillDir = repoDir / os.RelPath(skillSubpath)
    val skillMdPath = skillDir / "SKILL.md"

    if !os.exists(skillMdPath) then
      System.err.println(s"Error: SKILL.md not found at $skillSubpath".red)
      sys.exit(1)

    val content = os.read(skillMdPath)
    if !Yaml.hasValidFrontmatter(content) then
      System.err.println("Error: Invalid SKILL.md (missing YAML frontmatter)".red)
      sys.exit(1)

    val skillName = skillSubpath.split("/").last
    val targetPath = targetDir / skillName

    if !warnIfConflict(skillName, targetPath, isProject, options.yes) then
      println(s"Skipped: $skillName".yellow)
      return

    os.makeDir.all(targetDir)
    if !isPathInside(targetPath, targetDir) then
      System.err.println("Security error: Installation path outside target directory".red)
      sys.exit(1)

    os.copy(skillDir, targetPath, replaceExisting = true)
    SkillMetadata.writeSkillMetadata(targetPath, buildGitMetadata(sourceInfo, skillSubpath))

    println(s"\u2705 Installed: $skillName".green)
    println(s"   Location: $targetPath")

  private def installFromRepo(
    repoDir: os.Path,
    targetDir: os.Path,
    options: InstallOptions,
    repoName: Option[String],
    sourceInfo: InstallSourceInfo,
  ): Unit =
    val rootSkillPath = repoDir / "SKILL.md"

    final case class SkillInfo(
      skillDir: os.Path,
      skillName: String,
      description: String,
      targetPath: os.Path,
      size: Long,
    )

    var skillInfos: List[SkillInfo] =
      if os.exists(rootSkillPath) then
        val content = os.read(rootSkillPath)
        if !Yaml.hasValidFrontmatter(content) then
          System.err.println("Error: Invalid SKILL.md (missing YAML frontmatter)".red)
          sys.exit(1)

        val frontmatterName = Yaml.extractYamlField(content, "name")
        val skillName = if frontmatterName.nonEmpty then frontmatterName
                        else repoName.getOrElse(repoDir.last)
        List(SkillInfo(
          skillDir = repoDir,
          skillName = skillName,
          description = Yaml.extractYamlField(content, "description"),
          targetPath = targetDir / skillName,
          size = getDirectorySize(repoDir),
        ))
      else
        Nil

    if skillInfos.isEmpty then
      def findSkills(dir: os.Path): List[os.Path] =
        os.list(dir).toList.flatMap { entry =>
          if os.isDir(entry) then
            if os.exists(entry / "SKILL.md") then List(entry)
            else findSkills(entry)
          else Nil
        }

      val skillDirs = findSkills(repoDir)

      if skillDirs.isEmpty then
        System.err.println("Error: No SKILL.md files found in repository".red)
        sys.exit(1)

      skillInfos = skillDirs.flatMap { skillDir =>
        val skillMdPath = skillDir / "SKILL.md"
        val content = os.read(skillMdPath)

        if !Yaml.hasValidFrontmatter(content) then None
        else
          val skillName = skillDir.last
          val description = Yaml.extractYamlField(content, "description")
          val targetPath = targetDir / skillName
          val size = getDirectorySize(skillDir)
          Some(SkillInfo(skillDir, skillName, description, targetPath, size))
      }

      if skillInfos.isEmpty then
        System.err.println("Error: No valid SKILL.md files found".red)
        sys.exit(1)

    println(s"Found ${skillInfos.length} skill(s)\n".dim)

    var skillsToInstall = skillInfos

    if !options.yes && skillInfos.length > 1 then
      val labels = skillInfos.map { info =>
        s"${info.skillName.padTo(25, ' ').bold} ${formatSize(info.size).dim}"
      }

      val cancelled = Prompts.sync.use { prompts =>
        prompts.multiChoiceAllSelected("Select skills to install", labels) match
          case Completion.Finished(selectedLabels) =>
            if selectedLabels.isEmpty then
              println("No skills selected. Installation cancelled.".yellow)
              true
            else
              skillsToInstall = skillInfos.filter { info =>
                selectedLabels.exists(_.contains(info.skillName))
              }
              false

          case Completion.Fail(CompletionError.Interrupted) =>
            println("\n\nCancelled by user".yellow)
            sys.exit(0)

          case Completion.Fail(CompletionError.Error(msg)) =>
            System.err.println(s"Error: $msg")
            sys.exit(1)
      }
      if cancelled then return

    val isProject = targetDir.startsWith(os.pwd)
    var installedCount = 0

    for info <- skillsToInstall do
      if warnIfConflict(info.skillName, info.targetPath, isProject, options.yes) then
        os.makeDir.all(targetDir)
        if !isPathInside(info.targetPath, targetDir) then
          System.err.println("Security error: Installation path outside target directory".red)
        else
          os.copy(info.skillDir, info.targetPath, replaceExisting = true)
          SkillMetadata.writeSkillMetadata(
            info.targetPath,
            buildMetadataFromSource(sourceInfo, info.skillDir, repoDir),
          )
          println(s"\u2705 Installed: ${info.skillName}".green)
          installedCount += 1
      else
        println(s"Skipped: ${info.skillName}".yellow)

    println(s"\n\u2705 Installation complete: $installedCount skill(s) installed".green)

  private def buildMetadataFromSource(
    sourceInfo: InstallSourceInfo,
    skillDir: os.Path,
    repoDir: os.Path,
  ): SkillSourceMetadata =
    if sourceInfo.sourceType == SkillSourceType.Local then
      buildLocalMetadata(sourceInfo, skillDir)
    else
      val subpath = skillDir.relativeTo(repoDir).toString
      val normalizedSubpath = if subpath == "." then "" else subpath
      buildGitMetadata(sourceInfo, normalizedSubpath)

  private def buildGitMetadata(sourceInfo: InstallSourceInfo, subpath: String): SkillSourceMetadata =
    SkillSourceMetadata(
      source = sourceInfo.source,
      sourceType = SkillSourceType.Git,
      repoUrl = sourceInfo.repoUrl,
      subpath = Some(subpath),
      installedAt = aiskills.core.utils.isoNow(),
    )

  private def buildLocalMetadata(sourceInfo: InstallSourceInfo, skillDir: os.Path): SkillSourceMetadata =
    SkillSourceMetadata(
      source = sourceInfo.source,
      sourceType = SkillSourceType.Local,
      localPath = Some(skillDir.toString),
      installedAt = aiskills.core.utils.isoNow(),
    )

  private def warnIfConflict(
    skillName: String,
    targetPath: os.Path,
    isProject: Boolean,
    skipPrompt: Boolean,
  ): Boolean =
    if os.exists(targetPath) then
      if skipPrompt then
        println(s"Overwriting: $skillName (all existing files and folders will be removed)".dim)
        os.remove.all(targetPath)
        true
      else
        println(s"\u26a0 All existing files and folders in '$skillName' will be removed if you choose to overwrite.".yellow)
        Prompts.sync.use { prompts =>
          prompts.confirm(s"Skill '$skillName' already exists. Overwrite?".yellow, default = false) match
            case Completion.Finished(shouldOverwrite) =>
              if shouldOverwrite then os.remove.all(targetPath)
              shouldOverwrite
            case Completion.Fail(CompletionError.Interrupted) =>
              println("\n\nCancelled by user".yellow)
              sys.exit(0)
            case Completion.Fail(CompletionError.Error(_)) =>
              false
        }
    else
      if !isProject && MarketplaceSkills.anthropicMarketplaceSkills.contains(skillName) then
        System.err.println(s"\n\u26a0\ufe0f  Warning: '$skillName' matches an Anthropic marketplace skill".yellow)
        System.err.println("   Installing globally may conflict with Claude Code plugins.".dim)
        System.err.println("   If you re-enable Claude plugins, this will be overwritten.".dim)
        System.err.println("   Recommend: Use --project flag for conflict-free installation.\n".dim)
      true
