package aiskills.cli

import cats.syntax.all.*
import com.monovore.decline.*
import aiskills.core.{Agent, InstallOptions, ReadOptions, SyncOptions}
import aiskills.cli.commands.*

object Main {

  val command: Command[Unit] = buildCommand(
    name = "aiskills",
    header = """Universal skills loader for AI coding agents
               |
               |Manage reusable prompt skills for Claude, Cursor, Codex, Gemini,
               |Copilot, and other AI coding agents. Skills are markdown files
               |(SKILL.md) that can be installed from GitHub repos, Git URLs, or
               |local directories.
               |
               |Quick start:
               |  aiskills install anthropics/skills            # Install skills from a GitHub repo
               |  aiskills list                                 # See what's installed
               |  aiskills read commit                          # Output a skill (for AI agents)
               |  aiskills update                               # Update all skills from source
               |  aiskills sync --from claude --to cursor       # Copy skills between agents
               |  aiskills remove commit                        # Remove a skill
               |  aiskills manage                               # Interactive removal
               |""".stripMargin,
    version = "0.1.0",
    main = {

      val listCommand = Opts.subcommand(
        "list",
        """List all installed skills
          |
          |Shows all skills across all agents and locations (project + global),
          |grouped by agent with a summary count.
          |
          |Examples:
          |  aiskills list                  # Show all installed skills
          |""".stripMargin,
      ) {
        Opts.unit.map(_ => ListCmd.listSkills())
      }

      val installCommand = Opts.subcommand(
        "install",
        """Install skill from GitHub or Git URL
          |
          |Accepts GitHub shorthand (owner/repo), a specific skill path
          |(owner/repo/skill-name), a full Git URL (HTTPS or SSH), or a local
          |directory path. By default skills are installed into the project-local
          |.agents/skills directory for the universal agent.
          |
          |Examples:
          |  aiskills install anthropics/skills                     # All skills from a GitHub repo (project, universal)
          |  aiskills install anthropics/skills/commit              # Single skill by path
          |  aiskills install owner/repo --global                   # Install globally (~/.agents/skills)
          |  aiskills install owner/repo --agent universal          # Install into .agents/skills (default)
          |  aiskills install owner/repo --agent claude             # Install into .claude/skills
          |  aiskills install owner/repo --agent cursor             # Install into .cursor/skills
          |  aiskills install owner/repo --agent claude --global    # Install globally (~/.claude/skills)
          |  aiskills install owner/repo --all-agents               # Install into all agent directories
          |  aiskills install owner/repo --all-agents --global      # Install globally for all agents
          |  aiskills install owner/repo -y                         # Skip interactive selection, install all
          |  aiskills install https://github.com/owner/repo.git     # Full HTTPS Git URL
          |  aiskills install git@github.com:owner/repo.git         # SSH Git URL
          |  aiskills install ./local/skill-directory               # Install from a local directory
          |  aiskills install ~/my-skills/my-skill                  # Install from home-relative path
          |""".stripMargin,
      ) {
        val source    = Opts.argument[String](metavar = "source")
        val global    = Opts.flag("global", "Install globally (default: project install)", short = "g").orFalse
        val agent     = Opts
          .option[String](
            "agent",
            s"Target agent (${Agent.all.map(_.toString.toLowerCase).mkString(", ")})",
            short = "a",
          )
          .withDefault("universal")
        val allAgents = Opts.flag("all-agents", "Install to all agent directories").orFalse
        val yes       = Opts.flag("yes", "Skip interactive selection, install all skills found", short = "y").orFalse
        (source, global, agent, allAgents, yes).mapN { (src, g, a, all, y) =>
          Agent.fromString(a) match {
            case Some(agentEnum) =>
              Install.installSkill(src, InstallOptions(global = g, agent = agentEnum, allAgents = all, yes = y))
            case None =>
              System
                .err
                .println(
                  s"Error: Invalid agent '$a'. Valid agents: ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
                )
              sys.exit(1)
          }
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
          |  aiskills read commit --prefer claude          # Prefer Claude's version
          |  aiskills read commit --prefer cursor          # Prefer Cursor's version
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
          Update.updateSkills(names.toList)
        }
      }

      val syncCommand =
        Opts.subcommand(
          "sync",
          """Sync skills between agent directories
            |
            |Copies skills from one agent's directory to another. Without
            |arguments, runs an interactive wizard to pick source/target agents.
            |Use --from / --to for scripted usage, and --all-agents to broadcast
            |to every other agent directory.
            |
            |Examples:
            |  aiskills sync                                                    # Interactive wizard
            |  aiskills sync commit --from claude --to cursor                   # Sync one skill
            |  aiskills sync commit --from claude --all-agents                  # Sync one skill to all agents
            |  aiskills sync --from claude --to cursor                          # Sync all skills between two agents
            |  aiskills sync --from claude --all-agents                         # Sync all skills to all agents
            |  aiskills sync commit --from universal --to copilot               # Sync from universal to Copilot
            |  aiskills sync commit --from claude --to cursor -y                # Skip confirmation prompts
            |""".stripMargin,
        ) {
          val skillName = Opts.argument[String](metavar = "skill-name").orNone
          val from      = Opts.option[String]("from", "Source agent").orNone
          val to        = Opts.option[String]("to", "Target agent").orNone
          val allAgents = Opts.flag("all-agents", "Sync to all other agent directories").orFalse
          val yes       = Opts.flag("yes", "Skip confirmation", short = "y").orFalse
          (skillName, from, to, allAgents, yes).mapN { (sn, f, t, all, y) =>
            val fromAgent = f.flatMap(Agent.fromString)
            val toAgent   = t.flatMap(Agent.fromString)
            Sync.syncSkills(
              SyncOptions(
                skillName = sn,
                from = fromAgent,
                to = toAgent,
                allAgents = all,
                yes = y,
              )
            )
          }
        }

      val manageCommand = Opts.subcommand(
        "manage",
        """Interactively manage (remove) installed skills
          |
          |Opens a multi-select picker showing all installed skills. Select
          |the ones you want to remove. Useful when you need to clean up
          |several skills at once.
          |
          |Examples:
          |  aiskills manage                              # Interactive skill removal
          |""".stripMargin,
      ) {
        Opts.unit.map(_ => Manage.manageSkills())
      }

      val removeCommand = Opts.subcommand(
        "remove",
        """Remove a specific skill by name
          |
          |Removes a single installed skill. For removing multiple skills
          |interactively, use 'aiskills manage' instead.
          |
          |Examples:
          |  aiskills remove commit                       # Remove the 'commit' skill
          |  aiskills remove review-pr                    # Remove the 'review-pr' skill
          |""".stripMargin,
      ) {
        Opts.argument[String](metavar = "skill-name").map { name =>
          Remove.removeSkill(name)
        }
      }

      listCommand
        .orElse(installCommand)
        .orElse(readCommand)
        .orElse(updateCommand)
        .orElse(syncCommand)
        .orElse(manageCommand)
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
