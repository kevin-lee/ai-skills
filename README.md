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

`ai-skills` is a native CLI tool for managing reusable prompt skills for AI coding agents. It can install and manage AI agent skills (`SKILL.md`) and provides commands to `install`, `list`, `read`, `search`, `update`, `sync`, and `remove` skills across project-local and global directories for multiple AI agents.

Built with Scala 3 and Scala Native — compiles to a standalone binary with no JVM or Node.js runtime required.

## Please visit the [ai-skills website](https://ai-skills.kevinly.dev) for a usage guide.

***

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

## Please visit the website for a usage guide: https://ai-skills.kevinly.dev

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
