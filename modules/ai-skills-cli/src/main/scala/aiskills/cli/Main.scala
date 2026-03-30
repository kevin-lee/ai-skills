package aiskills.cli

import cats.syntax.all.*
import com.monovore.decline.*
import aiskills.core.{Agent, InstallOptions, ListOptions, ReadOptions, RemoveOptions, SyncOptions}
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
          |  aiskills list --agent claude               # Both scopes, Claude only
          |  aiskills list --agent claude,cursor        # Both scopes, Claude + Cursor
          |  aiskills list --agent claude --project     # Project skills, Claude only
          |  aiskills list --agent all                  # Both scopes, all agents (no prompt)
          |  aiskills list --agent all --global         # Global skills, all agents
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
          if p && g then {
            System.err.println("Error: Cannot use both --project and --global. Omit both to show all.")
            sys.exit(1)
          } else ()
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
          ListCmd.listSkills(ListOptions(project = p, global = g, agent = parsedAgents))
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
          |  aiskills install anthropics/skills                     # Interactive agent & location selection
          |  aiskills install anthropics/skills/skills/pdf          # Single skill by path (interactive)
          |  aiskills install owner/repo/skills/skill-name          # Single skill by path (interactive)
          |  aiskills install owner/repo --global                   # Install globally (interactive agent selection)
          |  aiskills install owner/repo --agent universal          # Install into .agents/skills (project)
          |  aiskills install owner/repo --agent claude             # Install into .claude/skills (project)
          |  aiskills install owner/repo --agent claude,cursor      # Install into Claude + Cursor (project)
          |  aiskills install owner/repo --agent cursor             # Install into .cursor/skills (project)
          |  aiskills install owner/repo --agent claude --global    # Install globally (~/.claude/skills)
          |  aiskills install owner/repo --agent all                # Install into all agent directories
          |  aiskills install owner/repo --agent all --global       # Install globally for all agents
          |  aiskills install owner/repo -y                         # Skip interactive selection, install all
          |  aiskills install https://github.com/owner/repo.git     # Full HTTPS Git URL
          |  aiskills install git@github.com:owner/repo.git         # SSH Git URL (Useful for private repos)
          |  aiskills install ./local/skill-directory               # Install from a local directory
          |  aiskills install ~/my-skills/my-skill                  # Install from home-relative path
          |""".stripMargin,
      ) {
        val source = Opts.argument[String](metavar = "source")
        val global = Opts.flag("global", "Install globally (default: project install)", short = "g").orFalse
        val agent  = Opts
          .option[String](
            "agent",
            s"Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            short = "a",
          )
          .orNone
        val yes    = Opts.flag("yes", "Skip interactive selection, install all skills found", short = "y").orFalse
        (source, global, agent, yes).mapN { (src, g, a, y) =>
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
          Install.installSkill(src, InstallOptions(global = g, agent = parsedAgents, yes = y))
        }
      }

      val readCommand = Opts.subcommand(
        "read",
        """Read skill(s) to stdout (for AI agents)
          |
          |Outputs the SKILL.md content for one or more skills. Searches
          |project-local directories first, then global. When a skill exists
          |in multiple agent directories, use --prefer to select a specific one.
          |
          |Examples:
          |  aiskills read commit                         # Read one skill
          |  aiskills read commit review-pr               # Read multiple skills
          |  aiskills read commit --prefer claude         # Prefer Claude's version
          |  aiskills read commit --prefer cursor         # Prefer Cursor's version
          |""".stripMargin,
      ) {
        val names  = Opts.arguments[String](metavar = "skill-names")
        val prefer = Opts.option[String]("prefer", "Prefer skills from this agent's directory").orNone
        (names, prefer).mapN { (ns, p) =>
          val preferAgent = p.flatMap(Agent.fromString)
          Read.readSkill(ns.toList, ReadOptions(prefer = preferAgent))
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
            |Use --from / --to for scripted usage, and --to all to broadcast
            |to every other agent directory.
            |
            |Examples:
            |  aiskills sync                                         # Interactive wizard
            |  aiskills sync commit --from claude --to cursor        # Sync one skill
            |  aiskills sync commit --from claude --to all           # Sync one skill to all agents
            |  aiskills sync --from claude --to cursor               # Sync all skills between two agents
            |  aiskills sync --from claude --to all                  # Sync all skills to all agents
            |  aiskills sync commit --from universal --to copilot    # Sync from universal to Copilot
            |  aiskills sync commit --from claude --to cursor -y     # Skip confirmation prompts
            |""".stripMargin,
        ) {
          val skillName = Opts.argument[String](metavar = "skill-name").orNone
          val from      = Opts.option[String]("from", "Source agent").orNone
          val to        = Opts
            .option[String](
              "to",
              s"Target agent(s), comma-separated or 'all' (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            )
            .orNone
          val yes       = Opts.flag("yes", "Skip confirmation", short = "y").orFalse
          (skillName, from, to, yes).mapN { (sn, f, t, y) =>
            val fromAgent                     = f.flatMap(Agent.fromString)
            val parsedTo: Option[List[Agent]] = t.map { toStr =>
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
            Sync.syncSkills(
              SyncOptions(
                skillName = sn,
                from = fromAgent,
                to = parsedTo,
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
              if !p && !g then {
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
              Remove.removeSkill(name, RemoveOptions(project = p, global = g, agent = parsedAgents.some))
          }
        }
      }

      listCommand
        .orElse(installCommand)
        .orElse(readCommand)
        .orElse(updateCommand)
        .orElse(syncCommand)
        .orElse(removeCommand)
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
