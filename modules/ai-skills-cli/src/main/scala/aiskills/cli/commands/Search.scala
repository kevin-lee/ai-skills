package aiskills.cli.commands

import aiskills.cli.CliDefaults
import aiskills.core.utils.{AgentsMd, Dirs, LocalSearch, MarketplaceSearch, SkillMetadata, Skills, TerminalWidth, Yaml}
import aiskills.core.*
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
          promptForMarketplaceAction() match {
            case Left(code) => sys.exit(code)
            case Right(action) => executeMarketplaceAction(action, selected)
          }
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
    case ReadSkill, InstallSkill, ListSkill
  }

  private def promptForMarketplaceAction(): Either[Int, MarketplaceAction] = {
    val options = List(
      "read    — Read skill content (clones repo temporarily)",
      "install — Install selected skill(s)",
      "list    — Display skill details",
    )
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice("What would you like to do?", options) match {
        case Completion.Finished(selected) =>
          if selected.startsWith("read") then MarketplaceAction.ReadSkill.asRight
          else if selected.startsWith("install") then MarketplaceAction.InstallSkill.asRight
          else MarketplaceAction.ListSkill.asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def executeMarketplaceAction(action: MarketplaceAction, selected: List[MarketplaceResult]): Unit =
    action match {
      case MarketplaceAction.ListSkill =>
        displayMarketplaceResults(selected)

      case MarketplaceAction.InstallSkill =>
        installMarketplaceSkills(selected)

      case MarketplaceAction.ReadSkill =>
        readMarketplaceSkills(selected)
    }

  private def readMarketplaceSkills(selected: List[MarketplaceResult]): Unit = {
    aiskills.cli.TempDirCleanup.ensureAtexitRegistered()

    for (result, idx) <- selected.zipWithIndex do {
      if idx > 0 then println(separator)

      val tempDir = os.home / s".aiskills-temp-${System.currentTimeMillis()}"
      os.makeDir.all(tempDir)
      aiskills.cli.TempDirCleanup.register(tempDir)

      val spinner = Spinner.createDefaultSideEffect(
        SpinnerConfig
          .default
          .withText(s"Cloning ${result.source}...")
          .withColor(Color.cyan)
          .withIndent(2),
      )
      val _       = spinner.start()

      Try {
        Install.cloneWithFallback(s"https://github.com/${result.source}", (tempDir / "repo").toString)
      } match {
        case scala.util.Failure(_) =>
          val _ = spinner.fail(Some(s"Failed to clone ${result.source}"))
          System.err.println(s"Could not clone repository: ${result.source}".red)

        case scala.util.Success(_) =>
          val _ = spinner.succeed(Some(s"Cloned ${result.source}"))

          val repoDir = tempDir / "repo"
          findSkillMd(repoDir, result.name) match {
            case Some(skillMdPath) =>
              val content = os.read(skillMdPath)
              println("       Reading:".bold + s" ${result.name.blue.bold}")
              println("        Source:".bold + s" ${result.source.yellow.bold} [${result.marketplace}]")
              println()
              println(content)
              println()
              println("Skill read:".bold + s" ${result.name.blue.bold}")

            case None =>
              System.err.println(s"SKILL.md not found for '${result.name}' in ${result.source}".yellow)
          }
      }

      aiskills.cli.TempDirCleanup.safeRemoveAll(tempDir)
      aiskills.cli.TempDirCleanup.unregister()
    }
  }

  private def installMarketplaceSkills(selected: List[MarketplaceResult]): Unit = {
    // 1. Prompt for agents and location once
    val agents = promptForAgents() match {
      case Left(code) => sys.exit(code)
      case Right(a) => a
    }
    {
      val locations = promptForInstallLocation(agents) match {
        case Left(code) => sys.exit(code)
        case Right(l) => l
      }

      // 2. Group by source to clone each repo only once
      val bySource = selected.groupBy(_.source)

      aiskills.cli.TempDirCleanup.ensureAtexitRegistered()

      import OverwritePrompt.BulkDecision

      val (installedCount, _) = bySource.foldLeft((0, BulkDecision.Undecided: BulkDecision)) {
        case ((count, bulk), (source, results)) =>
          val skillNames = results.map(_.name)
          val repoUrl    = s"https://github.com/$source"

          val tempDir = os.home / s".aiskills-temp-${System.currentTimeMillis()}"
          os.makeDir.all(tempDir)
          aiskills.cli.TempDirCleanup.register(tempDir)

          val spinner = Spinner.createDefaultSideEffect(
            SpinnerConfig
              .default
              .withText(s"Cloning $source...")
              .withColor(Color.cyan)
              .withIndent(2),
          )
          val _       = spinner.start()

          val cloneResult = Try {
            Install.cloneWithFallback(repoUrl, (tempDir / "repo").toString)
          }

          val (repoCount, newBulk) = cloneResult match {
            case scala.util.Failure(_) =>
              val _ = spinner.fail(Some(s"Failed to clone $source"))
              System.err.println(s"Could not clone repository: $source".red)
              (0, bulk)

            case scala.util.Success(actualUrl) =>
              val _ = spinner.succeed(Some(s"Cloned $source"))

              val repoDir = tempDir / "repo"

              // Build list of (skillDir, metadata) for all found skills, then install across all agent×location combos
              val skillEntries = skillNames.flatMap { name =>
                findSkillMd(repoDir, name) match {
                  case Some(skillMdPath) =>
                    val skillDir = skillMdPath / os.up
                    val subpath  = skillDir.relativeTo(repoDir).toString
                    val metadata = SkillSourceMetadata(
                      source = source,
                      sourceType = SkillSourceType.Git,
                      repoUrl = actualUrl.some,
                      subpath = subpath.some,
                      localPath = none[String],
                      installedAt = aiskills.core.utils.isoNow(),
                    )
                    List((skillDir, metadata))

                  case None =>
                    System.err.println(s"SKILL.md not found for '$name' in $source".yellow)
                    Nil
                }
              }

              // Install each skill across all agent×location combos, threading BulkDecision
              val installTargets = for {
                (skillDir, metadata) <- skillEntries
                agent                <- agents
                location             <- locations.toList
              } yield (skillDir, agent, location, metadata)

              installTargets.foldLeft((0, bulk)) {
                case ((skillCount, currentBulk), (skillDir, agent, location, metadata)) =>
                  val (installed, nextBulk) = installSingleSkill(skillDir, agent, location, metadata, currentBulk)
                  (skillCount + (if installed then 1 else 0), nextBulk)
              }
          }

          aiskills.cli.TempDirCleanup.safeRemoveAll(tempDir)
          aiskills.cli.TempDirCleanup.unregister()

          (count + repoCount, newBulk)
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
  }

  /** Install a single skill directory to a specific agent and location.
    * Returns (installed, nextBulkDecision).
    */
  private def installSingleSkill(
    skillDir: os.Path,
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

    val skillName     = skillDir.last
    val targetPath    = targetDir / skillName
    val locationLabel = s"(${location.toString.toLowerCase}, ${agent.toString})"

    os.makeDir.all(targetDir)

    val (installed, nextBulk) =
      if !Install.isPathInside(targetPath, targetDir) then {
        System.err.println("Security error: Installation path outside target directory".red)
        (false, bulk)
      } else if !os.exists(targetPath) then {
        copyAndWriteMetadata(skillDir, targetPath, metadata)
        println(s"\u2705 Installed: $skillName $locationLabel".green)
        (true, bulk)
      } else {
        bulk match {
          case BulkDecision.OverwriteAll =>
            overwriteAndInstall(skillDir, targetPath, skillName, locationLabel, metadata)
            (true, bulk)

          case BulkDecision.SkipAll =>
            println(s"Skipped: $skillName $locationLabel".yellow)
            (false, bulk)

          case BulkDecision.Undecided =>
            OverwritePrompt.askOverwriteChoice(
              skillName,
              s"Skill '$skillName' already exists at $locationLabel. What would you like to do?",
            ) match {
              case Left(code) =>
                sys.exit(code)

              case Right(OverwriteChoice.Yes) =>
                overwriteAndInstall(skillDir, targetPath, skillName, locationLabel, metadata)
                (true, BulkDecision.Undecided)

              case Right(OverwriteChoice.No) =>
                println(s"Skipped: $skillName $locationLabel".yellow)
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
                    println(s"\u2705 Installed: $skillName as $newName $locationLabel".green)
                    (true, BulkDecision.Undecided)
                }

              case Right(OverwriteChoice.YesToAll) =>
                overwriteAndInstall(skillDir, targetPath, skillName, locationLabel, metadata)
                (true, BulkDecision.OverwriteAll)

              case Right(OverwriteChoice.NoToAll) =>
                println(s"Skipped: $skillName $locationLabel".yellow)
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
    skillName: String,
    locationLabel: String,
    metadata: SkillSourceMetadata,
  ): Unit = {
    println(s"Overwriting: $skillName $locationLabel".dim)
    os.remove.all(targetPath)
    copyAndWriteMetadata(skillDir, targetPath, metadata)
    println(s"\u2705 Installed: $skillName $locationLabel".green)
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

  /** Find a SKILL.md in a repo by skill name.
    *
    * Matching strategy (in priority order):
    *  1. Direct path: repoDir/skillName/SKILL.md
    *  2. Under skills/: repoDir/skills/skillName/SKILL.md
    *  3. Directory name match (recursive): any dir whose name equals skillName
    *  4. YAML name match: any SKILL.md whose frontmatter `name` field matches (case-insensitive).
    *     This handles marketplace names like "pdf merge & split" where the directory is "pdf-merge-split".
    */
  private[commands] def findSkillMd(repoDir: os.Path, skillName: String): Option[os.Path] = {
    val directPath = repoDir / skillName / "SKILL.md"
    val skillsPath = repoDir / "skills" / skillName / "SKILL.md"

    if os.exists(directPath) then directPath.some
    else if os.exists(skillsPath) then skillsPath.some
    else {
      // Collect all SKILL.md files in the repo
      val allSkillMds = collectSkillMds(repoDir)

      // Try matching by directory name
      allSkillMds
        .find { md => (md / os.up).last === skillName }
        .orElse {
          // Try matching by YAML name field (case-insensitive)
          val normalizedQuery = skillName.toLowerCase.trim
          allSkillMds.find { md =>
            val content  = Try(os.read(md)).getOrElse("")
            val yamlName = Yaml.extractYamlField(content, "name")
            yamlName.toLowerCase.trim === normalizedQuery
          }
        }
    }
  }

  /** Collect all SKILL.md files under a directory (non-recursive into SKILL.md subdirs). */
  private[commands] def collectSkillMds(dir: os.Path): List[os.Path] =
    Try(os.list(dir)).toOption match {
      case None => Nil
      case Some(entries) =>
        entries.toList.flatMap { entry =>
          if Try(os.isDir(entry)).getOrElse(false) then {
            val md = entry / "SKILL.md"
            if os.exists(md) then List(md)
            else collectSkillMds(entry)
          } else Nil
        }
    }

  private def displayMarketplaceResults(results: List[MarketplaceResult]): Unit = {
    println("\nSearch Results:\n".bold)
    for result <- results do {
      val installLabel = formatInstalls(result.installs)
      println(s"  ${result.name.bold.padTo(25, ' ')} ${result.source.cyan}")
      if result.description.nonEmpty then println(s"    ${result.description.dim}")
      else ()
      println(s"    ${"Installs:".dim} $installLabel  ${"Marketplace:".dim} ${result.marketplace}")
      println()
    }
    println(s"${results.length} skill(s) shown".dim)
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
            promptForLocalAction() match {
              case Left(code) => sys.exit(code)
              case Right(action) => executeLocalAction(action, selected)
            }
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
    case ReadSkill, ListSkill
  }

  private def promptForLocalAction(): Either[Int, LocalAction] = {
    val options = List(
      "read — Read skill content",
      "list — Display skill details",
    )
    aiskills.cli.SigintHandler.install()
    Prompts.sync.use { prompts =>
      prompts.singleChoice("What would you like to do?", options) match {
        case Completion.Finished(selected) =>
          if selected.startsWith("read") then LocalAction.ReadSkill.asRight
          else LocalAction.ListSkill.asRight

        case Completion.Fail(CompletionError.Interrupted) =>
          println("\n\nCancelled by user".yellow)
          0.asLeft

        case Completion.Fail(CompletionError.Error(msg)) =>
          System.err.println(s"Error: $msg")
          1.asLeft
      }
    }
  }

  private def executeLocalAction(action: LocalAction, selected: List[Skill]): Unit =
    action match {
      case LocalAction.ListSkill =>
        displayLocalResults(selected)

      case LocalAction.ReadSkill =>
        for (skill, idx) <- selected.zipWithIndex do {
          if idx > 0 then println(separator)
          val skillPath = skill.path / "SKILL.md"
          val content   = os.read(skillPath)
          println("       Reading:".bold + s" ${skill.name.blue.bold}")
          println("Base directory:".bold + s" ${Dirs.displayPath(skill.path).yellow.bold}")
          println()
          println(content)
          println()
          println("Skill read:".bold + s" ${skill.name.blue.bold}")
        }
    }

  private def displayLocalResults(skills: List[Skill]): Unit = {
    println("\nSearch Results:\n".bold)

    val sorted = skills.sortBy(s => (s.agent.ordinal, s.location.ordinal, s.name))

    for skill <- sorted do {
      val pathLabel     = Dirs.displaySkillsDir(skill.agent, skill.location)
      val locationLabel =
        s"(${skill.location.toString.toLowerCase}, ${skill.agent.toString})".blue + s": $pathLabel".dim
      println(s"  ${skill.name.padTo(25, ' ').bold} $locationLabel")
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
