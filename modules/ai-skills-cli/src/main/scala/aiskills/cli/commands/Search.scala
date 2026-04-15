package aiskills.cli.commands

import aiskills.cli.CliDefaults
import aiskills.core.utils.{
  AgentsMd,
  Dirs,
  LocalSearch,
  MarketplaceSearch,
  SkillMdFinder,
  SkillMetadata,
  Skills,
  TerminalWidth,
  Yaml
}
import aiskills.core.*
import aiskills.core.given
import cats.syntax.all.*
import cue4s.*
import extras.scala.io.syntax.color.*
import just.spinner.*

import scala.util.Try

object Search {

  private val MinQueryLength = 2

  private def generateSeparator(separatorChar: Char) =
    "\n" + (separatorChar.toString * TerminalWidth.getTerminalWidth()).green.bold + "\n"

  private val separator = generateSeparator('=')

  // ── Entry points ──────────────────────────────────────────────────

  /** Full interactive mode: choose location, enter query, select results, choose action. */
  def searchInteractive(): Unit =
    promptForLocation() match {
      case Left(code) => sys.exit(code)
      case Right(location) =>
        promptForQuery() match {
          case Left(code) => sys.exit(code)
          case Right(query) =>
            location match {
              case SearchLocation.Marketplace => runMarketplaceSearch(query)
              case SearchLocation.Local => runLocalSearch(query)
            }
        }
    }

  /** Interactive with pre-filled query: choose location, search immediately. */
  def searchWithQuery(query: String): Unit =
    promptForLocation() match {
      case Left(code) => sys.exit(code)
      case Right(location) =>
        location match {
          case SearchLocation.Marketplace => runMarketplaceSearch(query)
          case SearchLocation.Local => runLocalSearch(query)
        }
    }

  /** Direct marketplace search. */
  def searchMarketplace(query: String): Unit =
    runMarketplaceSearch(query)

  /** Direct local search. */
  def searchLocal(query: String): Unit =
    runLocalSearch(query)

  // ── Marketplace search flow ───────────────────────────────────────

  private def runMarketplaceSearch(query: String): Unit = {
    println(s"Searching marketplaces for: ${query.cyan.bold}\n")

    val spinner = Spinner.createDefaultSideEffect(
      SpinnerConfig
        .default
        .withText("Searching skills.sh and agentskill.sh...")
        .withColor(Color.cyan)
        .withIndent(2),
    )
    val _       = spinner.start()

    val results = MarketplaceSearch.searchAll(query)

    if results.isEmpty then {
      val _ = spinner.fail(Some("No results found"))
      println(s"\nNo skills found matching '$query'.".yellow)
      println(s"Try a different search term or browse:")
      println(s"  ${"https://skills.sh".cyan}")
      println(s"  ${"https://agentskill.sh".cyan}")
    } else {
      val _ = spinner.succeed(Some(s"Found ${results.length} skill(s)"))
      println()

      promptForMarketplaceResults(results) match {
        case Left(code) => sys.exit(code)
        case Right(selected) =>
          runMarketplaceActionLoop(selected)
      }
    }
  }

  private def promptForMarketplaceResults(results: List[MarketplaceResult]): Either[Int, List[MarketplaceResult]] = {
    val labels = results.map { r =>
      val installLabel = formatInstalls(r.installs)
      s"${r.name.padTo(25, ' ')} ${r.source.padTo(30, ' ')} ${installLabel.padTo(12, ' ')} [${r.marketplace}]"
    }

    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.run(CliDefaults.mandatoryMultiChoiceNoneSelected("Select skill(s)", labels)) match {
        case Completion.Finished(selectedLabels) =>
          val selectedIndices = selectedLabels.flatMap { label =>
            labels.zipWithIndex.find { case (l, _) => l === label }.map { case (_, idx) => idx }
          }
          selectedIndices.map(results(_)).asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private enum MarketplaceAction {
    case ReadSkill, InstallSkill, ListSkill, Finish
  }

  private def promptForMarketplaceAction(): Either[Int, MarketplaceAction] = {
    val options = List(
      "list    — Display skill details",
      "read    — Read skill content",
      "install — Install selected skill(s)",
      "finish  — Finish",
    )
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice("What would you like to do?", options) match {
        case Completion.Finished(selected) =>
          if selected.startsWith("read") then MarketplaceAction.ReadSkill.asRight
          else if selected.startsWith("install") then MarketplaceAction.InstallSkill.asRight
          else if selected.startsWith("list") then MarketplaceAction.ListSkill.asRight
          else MarketplaceAction.Finish.asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  /** A marketplace skill that has been cloned (or attempted) for offline inspection. */
  final private case class ClonedSkill(
    result: MarketplaceResult,
    repoDir: os.Path,
    skillDir: os.Path,
    skillMdPath: Option[os.Path],
    content: String,
    description: String,
    yamlName: String,
    actualRepoUrl: String,
    tempDir: os.Path,
  )

  /** Clone each unique source once, build a ClonedSkill per selected entry.
    * On clone failure for a source: print a warning and drop skills from that source.
    */
  private def cloneSelected(selected: List[MarketplaceResult]): List[ClonedSkill] = {
    aiskills.cli.TempDirCleanup.ensureAtexitRegistered()

    val bySource = selected.groupBy(_.source)

    bySource.toList.flatMap {
      case (source, results) =>
        val repoUrl = s"https://github.com/$source"

        val tempDir = aiskills.cli.TempDirCleanup.createTempDir()

        val spinner = Spinner.createDefaultSideEffect(
          SpinnerConfig
            .default
            .withText(s"Cloning $source...")
            .withColor(Color.cyan)
            .withIndent(2),
        )
        val _       = spinner.start()

        Try {
          Install.cloneWithFallback(repoUrl, (tempDir / "repo").toString)
        } match {
          case scala.util.Failure(_) =>
            val _ = spinner.fail(Some(s"Failed to clone $source"))
            System.err.println(s"Could not clone repository: $source".red)
            aiskills.cli.TempDirCleanup.safeRemoveAll(tempDir)
            aiskills.cli.TempDirCleanup.unregister(tempDir)
            Nil

          case scala.util.Success(actualUrl) =>
            val _ = spinner.succeed(Some(s"Cloned $source"))

            val repoDir = tempDir / "repo"

            results.flatMap { result =>
              findSkillMd(repoDir, result.skillId, result.name) match {
                case Some(skillMdPath) =>
                  val skillDir    = skillMdPath / os.up
                  val content     = Try(os.read(skillMdPath)).getOrElse("")
                  val yamlName    = Yaml.extractYamlField(content, "name")
                  val description = Yaml.extractYamlField(content, "description")
                  List(
                    ClonedSkill(
                      result = result,
                      repoDir = repoDir,
                      skillDir = skillDir,
                      skillMdPath = skillMdPath.some,
                      content = content,
                      description = description,
                      yamlName = yamlName,
                      actualRepoUrl = actualUrl,
                      tempDir = tempDir,
                    )
                  )

                case None =>
                  val trimmedName = result.name.trim
                  val notFoundMsg =
                    if trimmedName.nonEmpty && trimmedName =!= result.skillId.trim
                    then s"SKILL.md not found for '${result.skillId}' (name: '${result.name}') in $source"
                    else s"SKILL.md not found for '${result.skillId}' in $source"
                  System.err.println(notFoundMsg.yellow)
                  List(
                    ClonedSkill(
                      result = result,
                      repoDir = repoDir,
                      skillDir = repoDir,
                      skillMdPath = none[os.Path],
                      content = "",
                      description = "",
                      yamlName = "",
                      actualRepoUrl = actualUrl,
                      tempDir = tempDir,
                    )
                  )
              }
            }
        }
    }
  }

  /** Remove and unregister all unique temp dirs from a cloned selection. */
  private def cleanupClonedTempDirs(cloned: List[ClonedSkill]): Unit = {
    cloned.map(_.tempDir).distinct.foreach { dir =>
      aiskills.cli.TempDirCleanup.safeRemoveAll(dir)
      aiskills.cli.TempDirCleanup.unregister(dir)
    }
  }

  /** Read action using pre-cloned SKILL.md content. */
  private def readMarketplaceSkillsEnriched(cloned: List[ClonedSkill]): Unit = {
    cloned.zipWithIndex.foreach {
      case (c, idx) =>
        if idx > 0 then println(separator)
        println(SkillDisplay.padLabel("Reading:").bold + s" ${c.result.name.blue.bold}")
        println(SkillDisplay.padLabel("Source:").bold + s" ${c.result.source.yellow.bold} [${c.result.marketplace}]")
        renderMarketplaceInfoBlock(c)
        println()
        if c.skillMdPath.isDefined && c.content.nonEmpty then println(c.content)
        else println("(SKILL.md not found)".yellow)
        println()
        println("Skill read:".bold + s" ${c.result.name.blue.bold}")
    }
  }

  /** Install action using pre-cloned skill dirs. Agents/locations prompted by the caller. */
  private def installMarketplaceSkillsEnriched(
    cloned: List[ClonedSkill],
    agents: List[Agent],
    locations: Set[SkillLocation],
  ): Unit = {
    import OverwritePrompt.BulkDecision

    val installable = cloned.filter(_.skillMdPath.isDefined)

    val skillEntries = installable.map { c =>
      val subpath     = c.skillDir.relativeTo(c.repoDir).toString
      val installName = resolveInstallName(c.skillDir, c.repoDir, c.yamlName, c.result.skillId)
      val metadata    = SkillSourceMetadata(
        source = c.result.source,
        sourceType = SkillSourceType.Git,
        repoUrl = c.actualRepoUrl.some,
        subpath = subpath.some,
        localPath = none[String],
        installedAt = aiskills.core.utils.isoNow(),
      )
      (c.skillDir, installName, metadata)
    }

    val installTargets = for {
      (skillDir, installName, metadata) <- skillEntries
      agent                             <- agents
      location                          <- locations.toList
    } yield (skillDir, installName, agent, location, metadata)

    val (installedCount, _) = installTargets.foldLeft((0, BulkDecision.Undecided: BulkDecision)) {
      case ((skillCount, currentBulk), (skillDir, installName, agent, location, metadata)) =>
        val (installed, nextBulk) =
          installSingleSkill(skillDir, installName, agent, location, metadata, currentBulk)
        (skillCount + (if installed then 1 else 0), nextBulk)
    }

    if installedCount > 0 then {
      println(s"\n\u2705 Installation complete: $installedCount skill(s) installed".green)
      println(s"\n${"Read skill:".dim} ${"aiskills read <skill-name>".cyan}")
      if locations.contains(SkillLocation.Project) then println(
        s"${"Sync to other agents:".dim} ${"aiskills sync <skill-name> --from <agent> --to <agent>".cyan}"
      )
      else ()
    } else ()
  }

  /** Action loop for marketplace: clone once on first action needing it, reuse thereafter. */
  private def runMarketplaceActionLoop(selected: List[MarketplaceResult]): Unit = {
    aiskills.cli.TempDirCleanup.ensureAtexitRegistered()

    def ensureCloned(current: Option[List[ClonedSkill]]): List[ClonedSkill] =
      current.getOrElse(cloneSelected(selected))

    def exitWithCleanup(current: Option[List[ClonedSkill]], code: Int): Unit = {
      current.foreach(cleanupClonedTempDirs)
      sys.exit(code)
    }

    @scala.annotation.tailrec
    def loop(current: Option[List[ClonedSkill]]): Option[List[ClonedSkill]] =
      promptForMarketplaceAction() match {
        case Left(code) =>
          exitWithCleanup(current, code)
          current

        case Right(MarketplaceAction.Finish) =>
          current

        case Right(MarketplaceAction.ReadSkill) =>
          val cloned = ensureCloned(current)
          readMarketplaceSkillsEnriched(cloned)
          loop(cloned.some)

        case Right(MarketplaceAction.ListSkill) =>
          val cloned = ensureCloned(current)
          displayMarketplaceResultsEnriched(cloned)
          loop(cloned.some)

        case Right(MarketplaceAction.InstallSkill) =>
          val agents    = promptForAgents() match {
            case Left(code) =>
              exitWithCleanup(current, code)
              Nil
            case Right(a) => a
          }
          val locations = promptForInstallLocation(agents) match {
            case Left(code) =>
              exitWithCleanup(current, code)
              Set.empty[SkillLocation]
            case Right(l) => l
          }
          val cloned    = ensureCloned(current)
          installMarketplaceSkillsEnriched(cloned, agents, locations)
          cloned.some
      }

    val finalCloned = loop(none[List[ClonedSkill]])
    finalCloned.foreach(cleanupClonedTempDirs)
  }

  /** Install a single skill directory to a specific agent and location.
    * Returns (installed, nextBulkDecision).
    */
  private def installSingleSkill(
    skillDir: os.Path,
    installName: String,
    agent: Agent,
    location: SkillLocation,
    metadata: SkillSourceMetadata,
    bulk: OverwritePrompt.BulkDecision,
  ): (Boolean, OverwritePrompt.BulkDecision) = {
    import OverwritePrompt.{BulkDecision, OverwriteChoice}

    val isProject = location === SkillLocation.Project
    val folder    =
      if isProject then s"${agent.projectDirName}/skills"
      else s"${agent.globalDirName}/skills"
    val targetDir =
      if isProject then os.pwd / os.RelPath(folder)
      else os.home / os.RelPath(folder)

    val skillName     = installName
    val subpath       = metadata.subpath.getOrElse("")
    val labeledName   = Install.skillNameWithSubpath(skillName, subpath)
    val targetPath    = targetDir / skillName
    val locationLabel = s"(${location.toString.toLowerCase}, ${agent.toString})"

    os.makeDir.all(targetDir)

    val (installed, nextBulk) =
      if !Install.isPathInside(targetPath, targetDir) then {
        System.err.println("Security error: Installation path outside target directory".red)
        (false, bulk)
      } else if !os.exists(targetPath) then {
        copyAndWriteMetadata(skillDir, targetPath, metadata)
        println(s"\u2705 Installed: $labeledName $locationLabel".green)
        (true, bulk)
      } else {
        bulk match {
          case BulkDecision.OverwriteAll =>
            overwriteAndInstall(skillDir, targetPath, labeledName, locationLabel, metadata)
            (true, bulk)

          case BulkDecision.SkipAll =>
            println(s"Skipped: $labeledName $locationLabel".yellow)
            (false, bulk)

          case BulkDecision.Undecided =>
            val existingSubpath = Install.existingSubpathLabel(targetPath)
            println(s"   Existing: ${Install.skillNameWithSubpath(skillName, existingSubpath)}".dim)
            println(s"   New:      $labeledName".dim)
            OverwritePrompt.askOverwriteChoice(
              skillName,
              s"Skill '$skillName' already exists at $locationLabel. Replace with '$labeledName'?",
            ) match {
              case Left(code) =>
                sys.exit(code)

              case Right(OverwriteChoice.Yes) =>
                overwriteAndInstall(skillDir, targetPath, labeledName, locationLabel, metadata)
                (true, BulkDecision.Undecided)

              case Right(OverwriteChoice.No) =>
                println(s"Skipped: $labeledName $locationLabel".yellow)
                (false, BulkDecision.Undecided)

              case Right(OverwriteChoice.Rename) =>
                OverwritePrompt.askNewSkillName(skillName, targetDir) match {
                  case Left(code) =>
                    sys.exit(code)
                  case Right(newName) =>
                    val newTargetPath = targetDir / newName
                    os.copy(skillDir, newTargetPath, replaceExisting = true)
                    val skillMdPath   = newTargetPath / "SKILL.md"
                    if os.exists(skillMdPath) then {
                      val content = os.read(skillMdPath)
                      val updated = Yaml.replaceYamlField(content, "name", newName)
                      os.write.over(skillMdPath, updated)
                    } else ()
                    SkillMetadata.writeSkillMetadata(newTargetPath, metadata.copy(name = newName.some))
                    println(s"\u2705 Installed: $labeledName as $newName $locationLabel".green)
                    (true, BulkDecision.Undecided)
                }

              case Right(OverwriteChoice.YesToAll) =>
                overwriteAndInstall(skillDir, targetPath, labeledName, locationLabel, metadata)
                (true, BulkDecision.OverwriteAll)

              case Right(OverwriteChoice.NoToAll) =>
                println(s"Skipped: $labeledName $locationLabel".yellow)
                (false, BulkDecision.SkipAll)
            }
        }
      }

    if installed then AgentsMd.updateAgentsMdForAgent(agent, location)
    else ()
    (installed, nextBulk)
  }

  private def copyAndWriteMetadata(
    skillDir: os.Path,
    targetPath: os.Path,
    metadata: SkillSourceMetadata,
  ): Unit = {
    os.copy(skillDir, targetPath, replaceExisting = true)
    SkillMetadata.writeSkillMetadata(targetPath, metadata)
  }

  private def overwriteAndInstall(
    skillDir: os.Path,
    targetPath: os.Path,
    labeledName: String,
    locationLabel: String,
    metadata: SkillSourceMetadata,
  ): Unit = {
    println(s"Overwriting: $labeledName $locationLabel".dim)
    os.remove.all(targetPath)
    copyAndWriteMetadata(skillDir, targetPath, metadata)
    println(s"\u2705 Installed: $labeledName $locationLabel".green)
  }

  private def promptForAgents(): Either[Int, List[Agent]] = {
    val agentLabels = Agent.all.map(_.toString)
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
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
  }

  private def promptForInstallLocation(agents: List[Agent]): Either[Int, Set[SkillLocation]] = {
    val projectPaths   = agents.map(_.projectDirName).distinct.mkString(", ")
    val globalPaths    = agents.map(a => s"~/${a.globalDirName}").distinct.mkString(", ")
    val locationLabels = List(
      s"global  ($globalPaths)",
      s"project ($projectPaths)",
      "both",
    )
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
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
  }

  /** Compute the install folder name for a skill discovered under a cloned repo.
    *
    * Preference order:
    *   - Root-level skill (`skillDir == repoDir`): YAML `name:` -> marketplaceName -> `skillDir.last`
    *   - Non-root skill: `skillDir.last` -> YAML `name:` -> marketplaceName
    *
    * Blank (whitespace-only) candidates are skipped. Falls back to `skillDir.last` if no candidate is non-blank.
    */
  private[commands] def resolveInstallName(
    skillDir: os.Path,
    repoDir: os.Path,
    yamlName: String,
    marketplaceName: String,
  ): String = {
    val dirName    = skillDir.last
    val isRoot     = skillDir === repoDir
    val candidates =
      if isRoot then List(yamlName, marketplaceName, dirName)
      else List(dirName, yamlName, marketplaceName)
    candidates.find(_.trim.nonEmpty).getOrElse(dirName)
  }

  private def alnumNormalize(s: String): String =
    s.toLowerCase.filter(_.isLetterOrDigit)

  /** Find a SKILL.md in a repo by marketplace skillId and display name.
    *
    * Matching strategy (in priority order):
    *  1. Direct path: repoDir/skillId/SKILL.md
    *  2. Under skills/: repoDir/skills/skillId/SKILL.md
    *  3. Directory name match (recursive): any dir whose name equals skillId, then displayName
    *  4. YAML name match (case-insensitive, trimmed): any SKILL.md whose frontmatter `name`
    *     matches skillId, then displayName.
    *  5. Alphanumeric-only fuzzy match (last resort): any SKILL.md whose directory name or YAML
    *     `name` collapses (lowercase, letters/digits only) to the same string as skillId or
    *     displayName. Returns None on ambiguity (more than one distinct SKILL.md matches).
    *
    * This handles marketplace names like "pdf-ocr-extraction" (where only displayName
    * "pdf ocr extraction" matches the YAML "PDF OCR Extraction") and "pdf-merge-&-split"
    * (where dir "pdf-merge-split" only matches after alphanumeric normalization).
    */
  private[commands] def findSkillMd(
    repoDir: os.Path,
    skillId: String,
    displayName: String,
  ): Option[os.Path] = {
    val directPath = repoDir / skillId / "SKILL.md"
    val skillsPath = repoDir / "skills" / skillId / "SKILL.md"

    if os.exists(directPath) then directPath.some
    else if os.exists(skillsPath) then skillsPath.some
    else {
      val allSkillMds = collectSkillMds(repoDir)

      // Tier 3: Directory name exact match (skillId first, then displayName)
      val tier3 = allSkillMds
        .find { md => (md / os.up).last === skillId }
        .orElse {
          allSkillMds.find { md => (md / os.up).last === displayName }
        }

      tier3.orElse {
        // Cache YAML name reads for tiers 4 and 5
        val yamlNames: Map[os.Path, String] = allSkillMds.map { md =>
          val content = Try(os.read(md)).getOrElse("")
          md -> Yaml.extractYamlField(content, "name")
        }.toMap

        // Tier 4: YAML name exact match (case-insensitive, trimmed), skillId first then displayName
        val skillIdNorm     = skillId.toLowerCase.trim
        val displayNameNorm = displayName.toLowerCase.trim
        val tier4           = allSkillMds
          .find { md => yamlNames(md).toLowerCase.trim === skillIdNorm }
          .orElse {
            allSkillMds.find { md => yamlNames(md).toLowerCase.trim === displayNameNorm }
          }

        tier4.orElse {
          // Tier 5: Alphanumeric-only fuzzy match, ambiguity returns None
          val skillIdAlnum     = alnumNormalize(skillId)
          val displayNameAlnum = alnumNormalize(displayName)
          val targets          = Set(skillIdAlnum, displayNameAlnum).filter(_.nonEmpty)

          if targets.isEmpty then none[os.Path]
          else {
            val candidates = allSkillMds.filter { md =>
              val dirAlnum  = alnumNormalize((md / os.up).last)
              val yamlAlnum = alnumNormalize(yamlNames(md))
              targets.contains(dirAlnum) || targets.contains(yamlAlnum)
            }
            candidates match {
              case single :: Nil => single.some
              case _ => none[os.Path]
            }
          }
        }
      }
    }
  }

  /** Collect every SKILL.md under a directory. Uses 'git ls-files' when the directory is a git working tree
    * (honors .gitignore and never enters .git); falls back to a filesystem walk that skips .git otherwise.
    */
  private[commands] def collectSkillMds(dir: os.Path): List[os.Path] =
    SkillMdFinder.listSkillMds(dir)

  /** Right-align a label to the column of `Base directory:` used by SkillDisplay. Same ColonCol = 17. */
  private val MarketplaceInfoColonCol = 17

  private def padMarketplaceLabel(label: String): String = {
    val pad = MarketplaceInfoColonCol - label.length
    (" " * pad) + label
  }

  /** Render the marketplace metadata block (sourceType/source/subpath/name) for a cloned skill.
    * Used by both marketplace list and marketplace read flows.
    */
  private def renderMarketplaceInfoBlock(c: ClonedSkill): Unit = {
    val r = c.result
    println(s"${padMarketplaceLabel("sourceType:").bold} git")
    if r.source.nonEmpty then println(s"${padMarketplaceLabel("source:").bold} ${r.source}")
    else ()

    val subpathStr     = c.skillDir.relativeTo(c.repoDir).toString
    val subpathDisplay =
      if subpathStr.isEmpty || subpathStr === "." then "<root>"
      else subpathStr
    println(s"${padMarketplaceLabel("subpath:").bold} $subpathDisplay")
    println(s"${padMarketplaceLabel("name:").bold} ${c.yamlName}")
  }

  /** Marketplace list display using SKILL.md data from a pre-cloned selection.
    * Omits the `Base directory:` line because the skill is not installed.
    */
  private def displayMarketplaceResultsEnriched(cloned: List[ClonedSkill]): Unit = {
    println("\nSearch Results:\n".bold)
    cloned.foreach { c =>
      val r            = c.result
      val installLabel = formatInstalls(r.installs)
      println(s"  ${r.name.bold.padTo(25, ' ')} ${r.source.cyan}")

      renderMarketplaceInfoBlock(c)

      val description =
        if c.description.nonEmpty then c.description
        else r.description
      if description.nonEmpty then println(s"    ${description.dim}")
      else ()
      println(s"    ${"Installs:".dim} $installLabel  ${"Marketplace:".dim} ${r.marketplace}")
      println()
    }
    println(s"${cloned.length} skill(s) shown".dim)
  }

  // ── Local search flow ─────────────────────────────────────────────

  private def runLocalSearch(query: String): Unit = {
    val allSkills = Skills.findAllSkills()

    if allSkills.isEmpty then {
      println("No skills installed.\n")
      println("Install skills:")
      println(s"  ${"aiskills install anthropics/skills".cyan}   ${"# Install from GitHub".dim}")
    } else {
      val results = LocalSearch.fuzzySearch(query, allSkills)

      if results.isEmpty then {
        println(s"No local skills found matching '$query'.".yellow)
        println(s"${allSkills.length} skill(s) installed. Try a different search term.".dim)
      } else {
        println(s"Found ${results.length} matching skill(s) for '${query.cyan.bold}':\n")

        promptForLocalResults(results) match {
          case Left(code) => sys.exit(code)
          case Right(selected) =>
            runLocalActionLoop(selected)
        }
      }
    }
  }

  private def promptForLocalResults(results: List[LocalSearchResult]): Either[Int, List[Skill]] = {
    val labels = results.map { r =>
      val pathLabel     = Dirs.displaySkillsDir(r.skill.agent, r.skill.location)
      val locationLabel = s"(${r.skill.location.toString.toLowerCase}, ${r.skill.agent.toString}): $pathLabel"
      s"${r.skill.name.padTo(25, ' ')} $locationLabel"
    }

    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.run(CliDefaults.mandatoryMultiChoiceNoneSelected("Select skill(s)", labels)) match {
        case Completion.Finished(selectedLabels) =>
          val selectedIndices = selectedLabels.flatMap { label =>
            labels.zipWithIndex.find { case (l, _) => l === label }.map { case (_, idx) => idx }
          }
          selectedIndices.map(i => results(i).skill).asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private enum LocalAction {
    case ReadSkill, ListSkill, Finish
  }

  private def promptForLocalAction(): Either[Int, LocalAction] = {
    val options = List(
      "list   — Display skill details",
      "read   — Read skill content",
      "finish — Finish",
    )
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice("What would you like to do?", options) match {
        case Completion.Finished(selected) =>
          if selected.startsWith("read") then LocalAction.ReadSkill.asRight
          else if selected.startsWith("list") then LocalAction.ListSkill.asRight
          else LocalAction.Finish.asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def readLocalSkills(selected: List[Skill]): Unit =
    selected.zipWithIndex.foreach {
      case (skill, idx) =>
        if idx > 0 then println(separator)
        val skillPath = skill.path / "SKILL.md"
        val content   = os.read(skillPath)
        println(SkillDisplay.padLabel("Reading:").bold + s" ${skill.name.blue.bold}")
        SkillDisplay.renderInfoBlock(skill.path)
        println()
        println(content)
        println()
        println("Skill read:".bold + s" ${skill.name.blue.bold}")
    }

  /** Action loop for local search: read / list / finish. */
  private def runLocalActionLoop(selected: List[Skill]): Unit = {
    @scala.annotation.tailrec
    def loop(): Unit =
      promptForLocalAction() match {
        case Left(code) =>
          sys.exit(code)
        case Right(LocalAction.Finish) =>
          ()
        case Right(LocalAction.ReadSkill) =>
          readLocalSkills(selected)
          loop()
        case Right(LocalAction.ListSkill) =>
          displayLocalResults(selected)
          loop()
      }

    loop()
  }

  private def displayLocalResults(skills: List[Skill]): Unit = {
    println("\nSearch Results:\n".bold)

    val sorted = skills.sortBy(s => (s.agent.ordinal, s.location.ordinal, s.name))

    sorted.foreach { skill =>
      val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
      val locationLabel =
        s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString})".blue + s": $pathLabel".dim
      println(s"  ${skill.name.padTo(25, ' ').bold} $locationLabel")
      SkillDisplay.renderInfoBlock(skill.path)
      println(s"    ${skill.description.dim}\n")
    }
    println(s"${skills.length} skill(s) shown".dim)
  }

  // ── Shared prompts ────────────────────────────────────────────────

  private def promptForLocation(): Either[Int, SearchLocation] = {
    val options = List("marketplace", "local")
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice("Search from", options) match {
        case Completion.Finished(selected) =>
          if selected === "marketplace" then SearchLocation.Marketplace.asRight
          else SearchLocation.Local.asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def promptForQuery(): Either[Int, String] = {
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.text(
        "Enter search query",
        _.validate { s =>
          if s.trim.length < MinQueryLength
          then Some(PromptError(s"Query must be at least $MinQueryLength characters"))
          else None
        },
      ) match {
        case Completion.Finished(query) =>
          query.trim.asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  // ── Utilities ─────────────────────────────────────────────────────

  private[commands] def formatInstalls(count: Long): String =
    if count >= 1_000_000 then f"${count / 1_000_000.0}%.1fM installs"
    else if count >= 1_000 then f"${count / 1_000.0}%.1fK installs"
    else s"$count installs"
}
