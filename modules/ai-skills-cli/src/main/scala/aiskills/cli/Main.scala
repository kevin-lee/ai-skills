package aiskills.cli

import cats.syntax.all.*
import com.monovore.decline.*
import aiskills.core.{Agent, InstallOptions, ListOptions, ReadOptions, RemoveOptions, SkillLocation, SyncOptions}
import aiskills.cli.commands.*
import aiskills.info.AiSkillsInfo

object Main {

  val command: Command[Unit] = buildCommand(
    name = "aiskills",
    header = """Universal skills loader for AI coding agents
               |
               |Manage reusable prompt skills for Claude, Cursor, Codex, Gemini,
               |Windsurf, Copilot, and other AI coding agents. Skills are markdown files
               |(SKILL.md) that can be installed from GitHub repos, Git URLs, or
               |local directories.
               |
               |Quick start:
               |  aiskills install anthropics/skills               # Install skills from a GitHub repo
               |  aiskills list                                    # See what's installed
               |  aiskills read commit                             # Output a skill (for AI agents)
               |  aiskills search commit                           # Search for skills
               |  aiskills update                                  # Update all skills from source
               |  aiskills sync --from claude --to cursor          # Copy skills between agents
               |  aiskills remove                                  # Interactive removal
               |  aiskills remove commit --agent claude --project  # Remove from specific location
               |""".stripMargin,
    version = AiSkillsInfo.version,
    main = {

      val listCommand = Opts.subcommand(
        "list",
        """List installed skills
          |
          |Shows installed skills with optional filtering by location and agent.
          |Without flags, an interactive prompt lets you choose scope and agents.
          |
          |Examples:
          |  aiskills list                              # Interactive scope & agent selection
          |  aiskills list --project                    # Project skills, all agents
          |  aiskills list --global                     # Global skills, all agents
          |  aiskills list --project --global           # Both scopes, all agents
          |  aiskills list --agent claude --project     # Project skills, Claude only
          |  aiskills list --agent all --project        # Project skills, all agents (no prompt)
          |  aiskills list --agent all --global         # Global skills, all agents
          |  aiskills list --agent all --project --global  # Both scopes, all agents
          |""".stripMargin,
      ) {
        val project = Opts.flag("project", "Show project skills only", short = "p").orFalse
        val global  = Opts.flag("global", "Show global skills only", short = "g").orFalse
        val agent   = Opts
          .option[String](
            "agent",
            s"Filter by agent (comma-separated or 'all': ${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            short = "a",
          )
          .orNone
        (project, global, agent).mapN { (p, g, a) =>
          val parsedAgents: Option[List[Agent]] = a.map { agentStr =>
            aiskills.core.utils.AgentNames.parseAgentNames(agentStr) match {
              case Right(agents) => agents
              case Left(invalid) =>
                System
                  .err
                  .println(
                    s"Error: Invalid agent '$invalid'. Valid agents: all, ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
                  )
                sys.exit(1)
            }
          }

          val locations: Set[SkillLocation] = Set.concat(
            if p then Set(SkillLocation.Project) else Set.empty,
            if g then Set(SkillLocation.Global) else Set.empty,
          )
          if parsedAgents.isDefined && locations.isEmpty then {
            System.err.println("Error: Must specify --project and/or --global when using --agent.")
            System.err.println()
            System.err.println("  Example: aiskills list --agent claude --project")
            sys.exit(1)
          } else ()

          ListCmd.listSkills(ListOptions(locations = locations, agent = parsedAgents))
        }
      }

      val installCommand = Opts.subcommand(
        "install",
        """Install skill from GitHub or Git URL
          |
          |Accepts GitHub shorthand (owner/repo), a specific skill path
          |(owner/repo/skill-name), a full Git URL (HTTPS or SSH), or a local
          |directory path. If no --agent is given, an interactive prompt lets
          |you choose the target agent(s) and location.
          |
          |Examples:
          |  aiskills install anthropics/skills                          # Interactive agent & location selection
          |  aiskills install anthropics/skills/skills/pdf               # Single skill by path (interactive)
          |  aiskills install owner/repo/skills/skill-name               # Single skill by path (interactive)
          |  aiskills install owner/repo --agent claude --project        # Project, Claude
          |  aiskills install owner/repo --agent claude --global         # Global, Claude (~/.claude/skills)
          |  aiskills install owner/repo --agent claude --project --global  # Both scopes, Claude
          |  aiskills install owner/repo --agent claude,cursor --project # Project, Claude + Cursor
          |  aiskills install owner/repo --agent all --project           # Project, all agents
          |  aiskills install owner/repo --agent all --global            # Global, all agents
          |  aiskills install owner/repo --agent all --project --global  # Both scopes, all agents
          |  aiskills install owner/repo -y                              # Skip interactive selection, install all
          |  aiskills install https://github.com/owner/repo.git          # Full HTTPS Git URL
          |  aiskills install git@github.com:owner/repo.git              # SSH Git URL (Useful for private repos)
          |  aiskills install ./local/skill-directory                    # Install from a local directory
          |  aiskills install ~/my-skills/my-skill                       # Install from home-relative path
          |""".stripMargin,
      ) {
        val source  = Opts.argument[String](metavar = "source")
        val project = Opts.flag("project", "Install to project scope (current directory)", short = "p").orFalse
        val global  = Opts.flag("global", "Install to global scope (home directory)", short = "g").orFalse
        val agent   = Opts
          .option[String](
            "agent",
            s"Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            short = "a",
          )
          .orNone
        val yes     = Opts.flag("yes", "Skip interactive selection, install all skills found", short = "y").orFalse
        (source, project, global, agent, yes).mapN { (src, p, g, a, y) =>
          val parsedAgents: Option[List[Agent]] = a.map { agentStr =>
            aiskills.core.utils.AgentNames.parseAgentNames(agentStr) match {
              case Right(agents) => agents
              case Left(invalid) =>
                System
                  .err
                  .println(
                    s"Error: Invalid agent '$invalid'. Valid agents: all, ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
                  )
                sys.exit(1)
            }
          }

          val locations: Set[SkillLocation] = Set.concat(
            if p then Set(SkillLocation.Project) else Set.empty,
            if g then Set(SkillLocation.Global) else Set.empty,
          )
          if parsedAgents.isDefined && locations.isEmpty then {
            System.err.println("Error: Must specify --project and/or --global when using --agent.")
            System.err.println()
            System.err.println("  Example: aiskills install owner/repo --agent claude --project")
            sys.exit(1)
          } else ()

          Install.installSkill(src, InstallOptions(locations = locations, agent = parsedAgents, yes = y))
        }
      }

      val readCommand = Opts.subcommand(
        "read",
        """Read skill(s) to stdout (for AI agents)
          |
          |Without arguments, opens an interactive prompt to select scope,
          |agents, and skills. With skill names, reads from the specified
          |agent(s) and location(s).
          |
          |Interactive mode:
          |  aiskills read                                              # Interactive scope, agent & skill selection
          |
          |Non-interactive mode (--project/--global and --agent are required):
          |  aiskills read commit --agent claude --project              # Project, Claude
          |  aiskills read commit --agent claude --global               # Global, Claude
          |  aiskills read commit --agent claude --project --global     # Both scopes, Claude
          |  aiskills read commit --agent claude,cursor --project       # Project, Claude + Cursor
          |  aiskills read commit --agent all --project                 # Project, all agents
          |  aiskills read commit --agent all --project --global        # Both scopes, all agents
          |""".stripMargin,
      ) {
        val names   = Opts.arguments[String](metavar = "skill-names").orEmpty
        val project = Opts.flag("project", "Read from project scope", short = "p").orFalse
        val global  = Opts.flag("global", "Read from global scope", short = "g").orFalse
        val agent   = Opts
          .option[String](
            "agent",
            s"Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            short = "a",
          )
          .orNone
        (names, project, global, agent).mapN { (ns, p, g, a) =>
          val parsedAgents: Option[List[Agent]] = a.map { agentStr =>
            aiskills.core.utils.AgentNames.parseAgentNames(agentStr) match {
              case Right(agents) => agents
              case Left(invalid) =>
                System
                  .err
                  .println(
                    s"Error: Invalid agent '$invalid'. Valid agents: all, ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
                  )
                sys.exit(1)
            }
          }

          val locations: Set[SkillLocation] = Set.concat(
            if p then Set(SkillLocation.Project) else Set.empty,
            if g then Set(SkillLocation.Global) else Set.empty,
          )

          val hasAnyFlag = locations.nonEmpty || parsedAgents.isDefined

          if ns.isEmpty then {
            if hasAnyFlag then {
              System.err.println("Error: Must specify skill name(s) when using --project, --global, or --agent.")
              System.err.println()
              System.err.println("  Example: aiskills read commit --agent claude --project")
              System.err.println()
              System.err.println("For interactive reading, run without any flags:")
              System.err.println("  aiskills read")
              sys.exit(1)
            } else ()
            Read.readInteractive()
          } else {
            if !hasAnyFlag then {
              System.err.println("Error: Must specify --project/--global and --agent when reading by name.")
              System.err.println()
              System.err.println("  --project (-p)  Read from project scope (current directory)")
              System.err.println("  --global  (-g)  Read from global scope (home directory)")
              System
                .err
                .println(
                  s"  --agent (-a)    Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})"
                )
              System.err.println()
              System.err.println("  Example: aiskills read commit --agent claude --project")
              sys.exit(1)
            } else if parsedAgents.isDefined && locations.isEmpty then {
              System.err.println("Error: Must specify --project and/or --global when using --agent.")
              System.err.println()
              System.err.println("  Example: aiskills read commit --agent claude --project")
              sys.exit(1)
            } else if parsedAgents.isEmpty && locations.nonEmpty then {
              System.err.println("Error: Must specify --agent when using --project/--global.")
              System.err.println()
              System
                .err
                .println(
                  s"  --agent (-a)  Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})"
                )
              System.err.println()
              System.err.println("  Example: aiskills read commit --agent claude --project")
              sys.exit(1)
            } else ()

            Read.readSkill(ns, ReadOptions(locations = locations, agent = parsedAgents))
          }
        }
      }

      val updateCommand = Opts.subcommand(
        "update",
        """Update installed skills from their source (default: all)
          |
          |Re-fetches skills from their original install source (Git repo or
          |local path). When no skill names are given, all installed skills
          |are updated. Skills without source metadata are skipped (re-install
          |them once to enable updates).
          |
          |Examples:
          |  aiskills update                              # Update all installed skills
          |  aiskills update commit                       # Update a single skill
          |  aiskills update commit review-pr             # Update specific skills
          |""".stripMargin,
      ) {
        Opts.arguments[String](metavar = "skill-names").orEmpty.map { names =>
          Update.updateSkills(names)
        }
      }

      val syncCommand =
        Opts.subcommand(
          "sync",
          """Sync skills between agent directories
            |
            |Copies skills from one agent's directory to another. Without
            |arguments, runs an interactive wizard to pick source/target agents.
            |Use --from / --to / --project / --global for scripted usage.
            |
            |Examples:
            |  aiskills sync                                                              # Interactive wizard
            |  aiskills sync --from project:claude --to cursor --project                  # All skills, project -> project
            |  aiskills sync --from global:claude --to cursor,windsurf --global           # All skills, global -> global
            |  aiskills sync --from project:claude --to all --project --global            # All skills, project -> both
            |  aiskills sync commit --from global:claude --to cursor --project --global   # One skill, global -> both
            |  aiskills sync a b c --from project:claude --to codex,gemini --project      # Multiple skills
            |  aiskills sync --from project:universal --to copilot --global -y            # Skip confirmation prompts
            |""".stripMargin,
        ) {
          val skillNames = Opts.arguments[String](metavar = "skill-names").orEmpty
          val from       = Opts
            .option[String]("from", "Source location and agent (format: <project|global>:<agent>)")
            .orNone
          val to         = Opts
            .option[String](
              "to",
              s"Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            )
            .orNone
          val project    = Opts.flag("project", "Sync to project scope (current directory)", short = "p").orFalse
          val global     = Opts.flag("global", "Sync to global scope (home directory)", short = "g").orFalse
          val yes        = Opts.flag("yes", "Skip confirmation", short = "y").orFalse
          (skillNames, from, to, project, global, yes).mapN { (sn, f, t, p, g, y) =>
            val parsedFrom: Option[(SkillLocation, Agent)] = f.map { fromStr =>
              fromStr.split(":", 2) match {
                case Array(locStr, agentStr) =>
                  val location = locStr.toLowerCase match {
                    case "project" => SkillLocation.Project
                    case "global" => SkillLocation.Global
                    case other =>
                      System.err.println(s"Error: Invalid source location '$other' in --from value.")
                      System.err.println()
                      System.err.println("  --from must be in the format <location>:<agent>")
                      System.err.println("  where <location> is 'project' or 'global'.")
                      System.err.println()
                      System.err.println("  Examples:")
                      System.err.println("    --from project:claude")
                      System.err.println("    --from global:universal")
                      sys.exit(1)
                  }
                  val agent    = Agent.fromString(agentStr).getOrElse {
                    System
                      .err
                      .println(
                        s"Error: Invalid agent '$agentStr' in --from value. Valid agents: ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
                      )
                    sys.exit(1)
                  }
                  (location, agent)
                case _ =>
                  System.err.println(s"Error: Invalid --from format '$fromStr'.")
                  System.err.println()
                  System.err.println("  --from must be in the format <location>:<agent>")
                  System.err.println("  where <location> is 'project' or 'global'.")
                  System.err.println()
                  System.err.println("  Examples:")
                  System.err.println("    --from project:claude")
                  System.err.println("    --from global:universal")
                  sys.exit(1)
              }
            }
            val parsedTo: Option[List[Agent]]              = t.map { toStr =>
              aiskills.core.utils.AgentNames.parseAgentNames(toStr) match {
                case Right(agents) => agents
                case Left(invalid) =>
                  System
                    .err
                    .println(
                      s"Error: Invalid agent '$invalid'. Valid agents: all, ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
                    )
                  sys.exit(1)
              }
            }

            val locations: Set[SkillLocation] = Set.concat(
              if p then Set(SkillLocation.Project) else Set.empty,
              if g then Set(SkillLocation.Global) else Set.empty,
            )
            if parsedFrom.isDefined && parsedTo.isDefined && locations.isEmpty then {
              System.err.println("Error: Must specify --project and/or --global when using --from/--to.")
              System.err.println()
              System.err.println("  --project (-p)  Sync to project scope (current directory)")
              System.err.println("  --global  (-g)  Sync to global scope (home directory)")
              System.err.println()
              System.err.println("  Example: aiskills sync --from project:claude --to cursor --project")
              sys.exit(1)
            } else ()

            Sync.syncSkills(
              SyncOptions(
                skillNames = sn,
                from = parsedFrom,
                to = parsedTo,
                targetLocations = locations,
                yes = y,
              )
            )
          }
        }

      val removeCommand = Opts.subcommand(
        "remove",
        """Remove installed skills
          |
          |Without a skill name, opens an interactive multi-select prompt showing
          |all installed skills. With a skill name, removes from the specified
          |agent(s) and location(s).
          |
          |Interactive mode:
          |  aiskills remove                                    # Interactive multi-select
          |
          |Non-interactive mode (--project/--global and --agent are required):
          |  aiskills remove commit --agent claude --project           # Project, Claude
          |  aiskills remove commit --agent claude,cursor --project    # Project, Claude, Cursor
          |  aiskills remove commit --agent claude --global            # Global, Claude
          |  aiskills remove commit --agent claude,cursor --global     # Global, Claude, Cursor
          |  aiskills remove commit --agent claude --project --global  # Both scopes, Claude
          |  aiskills remove commit --agent all --project              # Project, all agents
          |  aiskills remove commit --agent all --project --global     # Everywhere, all agents
          |""".stripMargin,
      ) {
        val skillName = Opts.argument[String](metavar = "skill-name").orNone
        val project   = Opts.flag("project", "Remove from project scope", short = "p").orFalse
        val global    = Opts.flag("global", "Remove from global scope", short = "g").orFalse
        val agent     = Opts
          .option[String](
            "agent",
            s"Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            short = "a",
          )
          .orNone
        (skillName, project, global, agent).mapN { (sn, p, g, a) =>
          sn match {
            case None =>
              if p || g || a.isDefined then {
                System.err.println("Error: Must specify a skill name when using --project, --global, or --agent.")
                System.err.println()
                System.err.println("  Example: aiskills remove commit --project --agent claude")
                System.err.println()
                System.err.println("For interactive removal, run without any flags:")
                System.err.println("  aiskills remove")
                sys.exit(1)
              } else ()
              Remove.removeInteractive()
            case Some(name) =>
              val locations: Set[SkillLocation] = Set.concat(
                if p then Set(SkillLocation.Project) else Set.empty,
                if g then Set(SkillLocation.Global) else Set.empty,
              )
              if locations.isEmpty then {
                System.err.println("Error: Must specify --project and/or --global when removing by name.")
                System.err.println()
                System.err.println("  --project (-p)  Remove from project scope (current directory)")
                System.err.println("  --global  (-g)  Remove from global scope (home directory)")
                System.err.println()
                System.err.println("  Example: aiskills remove commit --project --agent claude")
                sys.exit(1)
              } else ()
              if a.isEmpty then {
                System.err.println("Error: Must specify --agent when removing by name.")
                System.err.println()
                System
                  .err
                  .println(
                    s"  --agent (-a)  Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})"
                  )
                System.err.println()
                System.err.println("  Example: aiskills remove commit --project --agent claude")
                sys.exit(1)
              } else ()

              val parsedAgents: List[Agent] = a
                .map { agentStr =>
                  aiskills.core.utils.AgentNames.parseAgentNames(agentStr) match {
                    case Right(agents) => agents
                    case Left(invalid) =>
                      System
                        .err
                        .println(
                          s"Error: Invalid agent '$invalid'. Valid agents: all, ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
                        )
                      sys.exit(1)
                  }
                }
                .getOrElse(Nil)

              Remove.removeSkill(name, RemoveOptions(locations = locations, agent = parsedAgents.some))
          }
        }
      }

      val searchCommand = Opts.subcommand(
        "search",
        """Search for skills from marketplaces or local installations
          |
          |Search skills.sh and agentskill.sh marketplaces, or fuzzy-search
          |your locally installed skills by name and description.
          |
          |Interactive mode:
          |  aiskills search                              # Choose location, enter query interactively
          |  aiskills search <query>                      # Choose location, search immediately
          |
          |Non-interactive mode:
          |  aiskills search <query> --marketplace        # Search marketplaces (skills.sh, agentskill.sh)
          |  aiskills search <query> --local              # Search installed skills (fuzzy match)
          |""".stripMargin,
      ) {
        val query       = Opts.argument[String](metavar = "query").orNone
        val marketplace =
          Opts.flag("marketplace", "Search from marketplaces (skills.sh, agentskill.sh)", short = "m").orFalse
        val local       = Opts.flag("local", "Search from local installed skills", short = "l").orFalse
        (query, marketplace, local).mapN { (q, m, l) =>
          if m && l then {
            System.err.println("Error: --marketplace and --local are mutually exclusive.")
            System.err.println()
            System.err.println("  Use --marketplace to search from online marketplaces")
            System.err.println("  Use --local to search from installed skills")
            sys.exit(1)
          } else ()

          val hasLocationFlag = m || l

          if hasLocationFlag && q.isEmpty then {
            System.err.println("Error: Must specify a search query when using --marketplace or --local.")
            System.err.println()
            System.err.println("  Example: aiskills search commit --marketplace")
            System.err.println("  Example: aiskills search commit --local")
            System.err.println()
            System.err.println("For interactive search, run without flags:")
            System.err.println("  aiskills search")
            sys.exit(1)
          } else ()

          q match {
            case Some(query) if query.trim.length < 2 =>
              System.err.println("Error: Search query must be at least 2 characters.")
              sys.exit(1)

            case Some(query) if m =>
              Search.searchMarketplace(query.trim)

            case Some(query) if l =>
              Search.searchLocal(query.trim)

            case Some(query) =>
              Search.searchWithQuery(query.trim)

            case None =>
              Search.searchInteractive()
          }
        }
      }

      listCommand
        .orElse(installCommand)
        .orElse(readCommand)
        .orElse(updateCommand)
        .orElse(syncCommand)
        .orElse(removeCommand)
        .orElse(searchCommand)
    }
  )

  def buildCommand(
    name: String,
    header: String,
    main: Opts[Unit],
    version: String,
  ): Command[Unit] = {

    val showVersion =
      if (version.isEmpty) Opts.never
      else
        Opts
          .flag("version", "Print the version number and exit.", visibility = Visibility.Partial)
          .map(_ => println(version))

    Command(name, header, helpFlag = true)(showVersion.orElse(main))
  }

  def main(args: Array[String]): Unit = {
    SigintHandler.install()
    try {
      command.parse(PlatformApp.ambientArgs.getOrElse(args.toSeq), sys.env) match {
        case Left(help) =>
          System.err.println(renderHelp(help))
          if help.errors.nonEmpty then sys.exit(1)
          else ()
        case Right(_) => ()
      }
    } catch {
      case e: Install.SkillInstallException => sys.exit(e.exitCode)
    }
  }

  private val renderHelp: Help => String = {
    // There's a bug in HelpFormat.autoColors
    //    val helpFormat = HelpFormat.autoColors(sys.env)
    val helpFormat =
      if sys.env.get("NO_COLOR").fold("")(_.trim).isEmpty
      then HelpFormat.Colors
      else HelpFormat.Plain

    if helpFormat.colorsEnabled
    then help => help.render(helpFormat)
    else help => help.toString
  }
}
