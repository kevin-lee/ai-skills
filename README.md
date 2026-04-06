# ai-skills


<p align="center">

  <img src="https://ai-skills.kevinly.dev/img/ai-skills-all.svg"/>

</p>

<p align="center">
  <a href="https://github.com/kevin-lee/ai-skills/actions/workflows/build-native.yml" target="_blank"><img alt="[Scala Native] Build All" src="https://github.com/kevin-lee/ai-skills/actions/workflows/build-native.yml/badge.svg" /></a>
  <a href="https://github.com/kevin-lee/ai-skills/actions/workflows/release.yml" target="_blank"><img alt="Release" src="https://github.com/kevin-lee/ai-skills/actions/workflows/release.yml/badge.svg" /></a>
  <a href="https://ai-skills.kevinly.dev" target="_blank"><img alt="Hits" src="https://hits.sh/github.com/kevin-lee/ai-skills.svg"/></a>
  <img alt="GitHub Release" src="https://img.shields.io/github/v/release/kevin-lee/ai-skills">
</p>

`ai-skills` is a native CLI tool for managing reusable prompt skills for AI coding agents. It can install and manage AI agent skills (`SKILL.md`) and provides commands to `install`, `list`, `read`, `update`, `sync`, and `remove` skills across project-local and global directories for multiple AI agents.

Built with Scala 3 and Scala Native — compiles to a standalone binary with no JVM or Node.js runtime required.

**Please visit the [ai-skills website](https://ai-skills.kevinly.dev) for more information.**

## Supported Agents

| Agent         | Project Dir         | Global Dir                    |
|---------------|---------------------|-------------------------------|
| **Universal** | `.agents/skills/`   | `~/.agents/skills/`           |
| **Claude**    | `.claude/skills/`   | `~/.claude/skills/`           |
| **Cursor**    | `.cursor/skills/`   | `~/.cursor/skills/`           |
| **Codex**     | `.codex/skills/`    | `~/.codex/skills/`            |
| **Gemini**    | `.gemini/skills/`   | `~/.gemini/skills/`           |
| **Windsurf**  | `.windsurf/skills/` | `~/.codeium/windsurf/skills/` |
| **Copilot**   | `.github/skills/`   | `~/.copilot/skills/`          |

## Install

### Homebrew (macOS / Linux)

Install directly (taps automatically):

```sh
brew install kevin-lee/tap/ai-skills
```

Or tap first, then install:

Tap:
```sh
brew tap kevin-lee/tap
```
Then install:
```sh
brew install ai-skills
```

### Download from GitHub Releases

Pre-built binaries are available on the [Releases](https://github.com/kevin-lee/ai-skills/releases) page.

| Platform        | Binary                    |
|-----------------|---------------------------|
| macOS 26+ ARM64 | `aiskills-macos-26-arm64` |
| macOS 15  ARM64 | `aiskills-macos-15-arm64` |
| Linux ARM64     | `aiskills-linux-arm64`    |
| Linux x86_64    | `aiskills-linux-x86_64`   |

```sh
# Example: download and install on macOS ARM64
curl -L -o aiskills https://github.com/kevin-lee/ai-skills/releases/latest/download/aiskills-macos-26-arm64
chmod +x aiskills
sudo mv aiskills /usr/local/bin/
```

### Build from Source

See [Prerequisites](#prerequisites) and [Build](#build) below.

## Prerequisites

- [sbt](https://www.scala-sbt.org/) 1.12.5+
- [LLVM/Clang](https://scala-native.org/en/stable/user/setup.html) (required by Scala Native)
- Git (for installing skills from repositories)

## Build

### Development

```bash
sbt compile       # Compile all modules
sbt test          # Run all tests
sbt "cli/run list"  # Run CLI with arguments during development
```

### Native Binary

```bash
sbt cli/nativeLink
```

This produces a standalone native binary at:

```
modules/ai-skills-cli/target/scala-3.8.3/aiskills
```

Copy or symlink it to a location on your `PATH`:

```bash
cp modules/ai-skills-cli/target/scala-3.8.3/aiskills /usr/local/bin/aiskills
```

## Usage

```
aiskills <command> [options]
```

### Commands

#### `list` — List installed skills

```bash
aiskills list                                    # Interactive scope & agent selection
aiskills list --project                          # Project skills, all agents
aiskills list --global                           # Global skills, all agents
aiskills list --project --global                 # Both scopes, all agents
aiskills list --agent claude --project           # Project skills, Claude only
aiskills list --agent all --project              # Project skills, all agents (no prompt)
aiskills list --agent all --global               # Global skills, all agents
aiskills list --agent all --project --global     # Both scopes, all agents
```

Displays installed skills with their names, descriptions, agent, scope (project/global), and file system paths. Without flags, an interactive prompt lets you select the scope and agent(s). When using `--agent`, `--project` and/or `--global` must be specified.

Options:

| Flag                    | Description                                                                                       |
|-------------------------|---------------------------------------------------------------------------------------------------|
| `-p`, `--project`       | Show project skills only                                                                          |
| `-g`, `--global`        | Show global skills only                                                                           |
| `-a`, `--agent <names>` | Filter by agent(s), comma-separated or `all` (universal, claude, cursor, codex, gemini, windsurf, copilot) |

#### `install <source>` — Install skills

Install from GitHub shorthand:

```bash
aiskills install owner/repo                # All skills in the repo
aiskills install owner/repo/skill-name     # Specific skill
```

Install from a Git URL:

```bash
aiskills install https://github.com/owner/repo.git
aiskills install git@github.com:owner/repo.git         # Useful for private repos
```

Install from a local path:

```bash
aiskills install ./path/to/skill-directory
aiskills install ~/my-skills/my-skill
```

Target specific agent(s) and location(s):

```bash
aiskills install owner/repo                                    # Interactive agent & location selection
aiskills install owner/repo --agent claude --project           # Project, Claude
aiskills install owner/repo --agent claude --global            # Global, Claude
aiskills install owner/repo --agent claude --project --global  # Both scopes, Claude
aiskills install owner/repo --agent claude,cursor --project    # Project, Claude + Cursor
aiskills install owner/repo --agent all --project              # Project, all agents
aiskills install owner/repo --agent all --global               # Global, all agents
aiskills install owner/repo --agent all --project --global     # Both scopes, all agents
```

Without `--agent`, an interactive prompt lets you choose the target agent(s) and location. If a skill already exists, you are prompted to overwrite or skip. When using `--agent`, `--project` and/or `--global` must be specified.

Options:

| Flag                    | Description                                                                                    |
|-------------------------|------------------------------------------------------------------------------------------------|
| `-a`, `--agent <names>` | Target agent(s), comma-separated or `all` (universal, claude, cursor, codex, gemini, windsurf, copilot) |
| `-p`, `--project`       | Install to project scope (current directory)                                                   |
| `-g`, `--global`        | Install to global scope (home directory)                                                       |
| `-y`, `--yes`           | Skip interactive selection, install all skills found                                           |

#### `read [skill-names]` — Read skills to stdout

Without arguments, opens an interactive prompt to select scope, agents, and skills. With skill names, reads from the specified agent(s) and location(s). In non-interactive mode, `--project`/`--global` and `--agent` are all required.

```bash
aiskills read                                              # Interactive scope, agent & skill selection
aiskills read commit --agent claude --project              # Project, Claude
aiskills read commit --agent claude --global               # Global, Claude
aiskills read commit --agent claude --project --global     # Both scopes, Claude
aiskills read commit --agent claude,cursor --project       # Project, Claude + Cursor
aiskills read commit --agent all --project                 # Project, all agents
aiskills read commit --agent all --project --global        # Both scopes, all agents
```

Outputs the SKILL.md content. Intended for AI agents to consume skill definitions.

Options:

| Flag                    | Description                                                                                    |
|-------------------------|------------------------------------------------------------------------------------------------|
| `-a`, `--agent <names>` | Target agent(s), comma-separated or `all` (universal, claude, cursor, codex, gemini, windsurf, copilot) |
| `-p`, `--project`       | Read from project scope (current directory)                                                    |
| `-g`, `--global`        | Read from global scope (home directory)                                                        |

#### `update [skill-names]` — Update installed skills

```bash
aiskills update             # Update all skills
aiskills update my-skill    # Update specific skill(s)
```

Re-fetches skills from their original source (git or local).

#### `sync` — Sync skills between agents

Copy skills from one agent's directory to another. If a skill already exists in the target, you are prompted to overwrite or skip. In non-interactive mode, `--from`, `--to`, and `--project`/`--global` are all required.

```bash
aiskills sync                                                                 # Interactive mode
aiskills sync --from project:claude --to cursor --project                     # All skills, project -> project
aiskills sync --from global:claude --to cursor,windsurf --global              # All skills, global -> global
aiskills sync --from project:claude --to all --project --global               # All skills, project -> both
aiskills sync commit --from global:claude --to cursor --project --global      # One skill, global -> both
aiskills sync skill1 skill2 skill3 --from global:claude --to codex --project  # Multiple skills
aiskills sync --from project:universal --to copilot --global -y               # Skip confirmation prompts
```

Options:

| Flag                        | Description                                                                                           |
|-----------------------------|-------------------------------------------------------------------------------------------------------|
| `--from <location>:<agent>` | Source location and agent (e.g., `project:claude`, `global:universal`)                                |
| `--to <agent>`              | Target agent(s), comma-separated or `all` (universal, claude, cursor, codex, gemini, windsurf, copilot) |
| `-p`, `--project`           | Sync to project scope (current directory)                                                             |
| `-g`, `--global`            | Sync to global scope (home directory)                                                                 |
| `-y`, `--yes`               | Skip confirmation prompts                                                                             |

#### `remove [skill-name]` — Remove installed skills

Without a skill name, opens an interactive multi-select prompt. With a skill name, `--project`/`--global` and `--agent` are all required.

```bash
aiskills remove                                                  # Interactive multi-select
aiskills remove commit --agent claude --project                  # Project, Claude
aiskills remove commit --agent claude --global                   # Global, Claude
aiskills remove commit --agent claude,cursor --project           # Project, Claude + Cursor
aiskills remove commit --agent claude,cursor,windsurf --global   # Global, Claude, Cursor, Windsurf
aiskills remove commit --agent claude --project --global         # Both scopes, Claude
aiskills remove commit --agent all --project                     # Project, all agents
aiskills remove commit --agent all --project --global            # Remove everywhere
```

Validation errors (all three are required together in non-interactive mode):

```
aiskills remove commit                    # ERROR: must specify --project/--global
aiskills remove commit --project          # ERROR: must specify --agent
aiskills remove --agent claude --project  # ERROR: must specify skill name
```

Options:

| Flag                    | Description                                                                                             |
|-------------------------|---------------------------------------------------------------------------------------------------------|
| `-p`, `--project`       | Remove from project scope                                                                               |
| `-g`, `--global`        | Remove from global scope                                                                                |
| `-a`, `--agent <names>` | Target agent(s), comma-separated or `all` (universal, claude, cursor, codex, gemini, windsurf, copilot) |

## Skill Format

Each skill is a directory containing a `SKILL.md` file with YAML frontmatter:

```
my-skill/
├── SKILL.md
└── (optional additional files)
```

`SKILL.md` format:

```markdown
---
name: My Skill
description: What this skill does
---

Instructions and context for the AI agent...
```

## Skill Directories

Skills are searched in the following directories, in priority order:

| Priority | Path                            | Scope              |
|----------|---------------------------------|--------------------|
| 1        | `./.agents/skills/`             | Project universal  |
| 2        | `./.claude/skills/`             | Project Claude     |
| 3        | `./.codex/skills/`              | Project Codex      |
| 4        | `./.github/skills/`             | Project Copilot    |
| 5        | `./.cursor/skills/`             | Project Cursor     |
| 6        | `./.gemini/skills/`             | Project Gemini     |
| 7        | `./.windsurf/skills/`           | Project Windsurf   |
| 8        | `~/.agents/skills/`             | Global universal   |
| 9        | `~/.claude/skills/`             | Global Claude      |
| 10       | `~/.codex/skills/`              | Global Codex       |
| 11       | `~/.copilot/skills/`            | Global Copilot     |
| 12       | `~/.cursor/skills/`             | Global Cursor      |
| 13       | `~/.gemini/skills/`             | Global Gemini      |
| 14       | `~/.codeium/windsurf/skills/`   | Global Windsurf    |

Skills in multiple directories are all shown by `list`. For `read`, use `--agent` with `--project`/`--global` to target specific directories.

## Project Structure

```
├── modules/
│   ├── ai-skills-core/  # Domain types, skill discovery, YAML parsing, metadata I/O
│   └── ai-skills-cli/   # CLI entry point and commands (decline + cue4s)
├── build.sbt
└── project/
    ├── build.properties
    └── plugins.sbt
```

## License

See [LICENSE](LICENSE) for details.
