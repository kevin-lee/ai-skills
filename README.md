# ai-skills

A native CLI tool for managing skills for AI coding agents. It implements the SKILL.md format and provides commands to install, list, read, update, sync, and remove skills across project-local and global directories for multiple AI agents.

Built with Scala 3 and Scala Native — compiles to a standalone binary with no JVM or Node.js runtime required.

## Supported Agents

| Agent                   | Project Dir       | Global Dir           |
|-------------------------|-------------------|----------------------|
| **Universal** (default) | `.agents/skills/` | `~/.agents/skills/`  |
| **Claude**              | `.claude/skills/` | `~/.claude/skills/`  |
| **Cursor**              | `.cursor/skills/` | `~/.cursor/skills/`  |
| **Codex**               | `.codex/skills/`  | `~/.codex/skills/`   |
| **Gemini**              | `.gemini/skills/` | `~/.gemini/skills/`  |
| **Copilot**             | `.github/skills/` | `~/.copilot/skills/` |

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
modules/ai-skills-cli/target/scala-3.8.2/aiskills
```

Copy or symlink it to a location on your `PATH`:

```bash
cp modules/ai-skills-cli/target/scala-3.8.2/aiskills /usr/local/bin/aiskills
```

## Usage

```
aiskills <command> [options]
```

### Commands

#### `list` — List all installed skills

```bash
aiskills list
```

Searches all agent skill directories and displays installed skills with their names, descriptions, agent, and scope (project/global).

#### `install <source>` — Install skills

Install from GitHub shorthand:

```bash
aiskills install owner/repo                # All skills in the repo
aiskills install owner/repo/skill-name     # Specific skill
```

Install from a Git URL:

```bash
aiskills install https://github.com/owner/repo.git
aiskills install git@github.com:owner/repo.git
```

Install from a local path:

```bash
aiskills install ./path/to/skill-directory
aiskills install ~/my-skills/my-skill
```

Target a specific agent or all agents:

```bash
aiskills install owner/repo                       # Default: universal (.agents/skills/)
aiskills install owner/repo --agent claude         # Claude only
aiskills install owner/repo --agent cursor         # Cursor only
aiskills install owner/repo --all-agents           # All agent directories
aiskills install owner/repo --agent gemini --global  # Global Gemini
```

Options:

| Flag                   | Description                                                                          |
|------------------------|--------------------------------------------------------------------------------------|
| `-a`, `--agent <name>` | Target agent (universal, claude, cursor, codex, gemini, copilot). Default: universal |
| `--all-agents`         | Install to all agent directories                                                     |
| `-g`, `--global`       | Install globally instead of project-local                                            |
| `-y`, `--yes`          | Skip interactive selection, install all skills found                                 |

#### `read <skill-names>` — Read skills to stdout

```bash
aiskills read my-skill
aiskills read skill-a skill-b
aiskills read my-skill --prefer cursor    # Prefer Cursor's copy if duplicates exist
```

Outputs the SKILL.md content. Intended for AI agents to consume skill definitions.

If the same skill exists in multiple agent directories, the first match by priority order is used. A note is printed to stderr listing alternative locations.

Options:

| Flag               | Description                               |
|--------------------|-------------------------------------------|
| `--prefer <agent>` | Prefer skills from this agent's directory |

#### `update [skill-names]` — Update installed skills

```bash
aiskills update             # Update all skills
aiskills update my-skill    # Update specific skill(s)
```

Re-fetches skills from their original source (git or local).

#### `sync` — Sync skills between agents

Copy skills from one agent's directory to another:

```bash
aiskills sync                                              # Interactive mode
aiskills sync my-skill --from claude --to cursor           # One skill, one target
aiskills sync my-skill --from claude --all-agents          # One skill, all other agents
aiskills sync --from claude --to cursor                    # All skills from Claude to Cursor
aiskills sync --from claude --all-agents                   # All Claude skills to all others
```

Options:

| Flag             | Description                         |
|------------------|-------------------------------------|
| `--from <agent>` | Source agent                        |
| `--to <agent>`   | Target agent                        |
| `--all-agents`   | Sync to all other agent directories |
| `-y`, `--yes`    | Skip confirmation prompts           |

#### `manage` — Interactive skill management

```bash
aiskills manage
```

Opens an interactive multi-select prompt to choose and remove installed skills.

#### `remove <skill-name>` — Remove a skill

```bash
aiskills remove my-skill
```

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

| Priority | Path                  | Scope              |
|----------|-----------------------|--------------------|
| 1        | `./.agents/skills/`   | Project universal  |
| 2        | `./.claude/skills/`   | Project Claude     |
| 3        | `./.codex/skills/`    | Project Codex      |
| 4        | `./.github/skills/`   | Project Copilot    |
| 5        | `./.cursor/skills/`   | Project Cursor     |
| 6        | `./.gemini/skills/`   | Project Gemini     |
| 7        | `~/.agents/skills/`   | Global universal   |
| 8        | `~/.claude/skills/`   | Global Claude      |
| 9        | `~/.codex/skills/`    | Global Codex       |
| 10       | `~/.copilot/skills/`  | Global Copilot     |
| 11       | `~/.cursor/skills/`   | Global Cursor      |
| 12       | `~/.gemini/skills/`   | Global Gemini      |

Skills in multiple directories are all shown by `list`. For `read`, the first match by priority is used (override with `--prefer`).

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
