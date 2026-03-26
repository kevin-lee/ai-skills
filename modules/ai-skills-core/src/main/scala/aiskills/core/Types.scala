package aiskills.core

import cats.syntax.either.*
import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}

enum Agent(val projectDirName: String, val globalDirName: String) {
  case Universal extends Agent(".agents", ".agents")
  case Claude extends Agent(".claude", ".claude")
  case Cursor extends Agent(".cursor", ".cursor")
  case Codex extends Agent(".codex", ".codex")
  case Gemini extends Agent(".gemini", ".gemini")
  case Windsurf extends Agent(".windsurf", ".codeium/windsurf")
  case Copilot extends Agent(".github", ".copilot")
}

object Agent {
  val all: List[Agent] = Agent.values.toList

  val allNonUniversal: List[Agent] = all.filterNot(_ == Agent.Universal)

  def fromString(s: String): Option[Agent] =
    all.find(_.toString.equalsIgnoreCase(s))

  def needsAgentsMd(agent: Agent): Boolean =
    agent match {
      case Agent.Universal | Agent.Codex => true
      case _ => false
    }

  given Encoder[Agent] = Encoder.encodeString.contramap(_.toString.toLowerCase)

  given Decoder[Agent] = Decoder.decodeString.emap { s =>
    fromString(s).toRight(s"Invalid Agent: $s. Valid agents: ${all.map(_.toString.toLowerCase).mkString(", ")}")
  }
}

enum SkillLocation {
  case Project, Global
}

object SkillLocation {
  given Encoder[SkillLocation] = Encoder.encodeString.contramap {
    case SkillLocation.Project => "project"
    case SkillLocation.Global => "global"
  }

  given Decoder[SkillLocation] = Decoder.decodeString.emap {
    case "project" => SkillLocation.Project.asRight
    case "global" => SkillLocation.Global.asRight
    case other => s"Invalid SkillLocation: $other".asLeft
  }
}

final case class Skill(
  name: String,
  description: String,
  location: SkillLocation,
  agent: Agent,
  path: os.Path,
)

final case class SkillLocationInfo(
  path: os.Path,
  baseDir: os.Path,
  source: os.Path,
  agent: Agent,
  location: SkillLocation,
)

final case class InstallOptions(
  global: Boolean = false,
  agent: Agent = Agent.Universal,
  allAgents: Boolean = false,
  yes: Boolean = false,
)

final case class ReadOptions(
  prefer: Option[Agent] = None,
)

final case class SkillMetadata(
  name: String,
  description: String,
  context: Option[String] = None,
)

enum SkillSourceType {
  case Git, GitHub, Local
}

object SkillSourceType {
  given Encoder[SkillSourceType] = Encoder.encodeString.contramap {
    case SkillSourceType.Git => "git"
    case SkillSourceType.GitHub => "github"
    case SkillSourceType.Local => "local"
  }

  given Decoder[SkillSourceType] = Decoder.decodeString.emap {
    case "git" => SkillSourceType.Git.asRight
    case "github" => SkillSourceType.GitHub.asRight
    case "local" => SkillSourceType.Local.asRight
    case other => s"Invalid SkillSourceType: $other".asLeft
  }
}

final case class SkillSourceMetadata(
  source: String,
  sourceType: SkillSourceType,
  repoUrl: Option[String] = None,
  subpath: Option[String] = None,
  localPath: Option[String] = None,
  installedAt: String,
)

object SkillSourceMetadata {
  given Encoder[SkillSourceMetadata] = deriveEncoder[SkillSourceMetadata]
  given Decoder[SkillSourceMetadata] = deriveDecoder[SkillSourceMetadata]
}

sealed trait AiSkillsError

object AiSkillsError {
  final case class SkillNotFound(name: String) extends AiSkillsError
  final case class GitCloneError(url: String, detail: String) extends AiSkillsError
  final case class MetadataParseError(path: os.Path, detail: String) extends AiSkillsError
  final case class InvalidFrontmatter(path: os.Path) extends AiSkillsError
  final case class InvalidSource(source: String) extends AiSkillsError
  final case class PathTraversalError(target: os.Path, parent: os.Path) extends AiSkillsError
  final case class InvalidOutputPath(path: String) extends AiSkillsError
  final case class IoError(detail: String) extends AiSkillsError
  final case class InvalidAgent(name: String) extends AiSkillsError

  extension (error: AiSkillsError) {
    def message: String = error match {
      case SkillNotFound(name) => s"Skill '$name' not found"
      case GitCloneError(url, detail) => s"Failed to clone repository: $url ($detail)"
      case MetadataParseError(path, detail) => s"Failed to parse metadata at $path: $detail"
      case InvalidFrontmatter(path) => s"Invalid SKILL.md (missing YAML frontmatter) at $path"
      case InvalidSource(source) => s"Invalid source format: $source"
      case PathTraversalError(target, parent) =>
        s"Security error: Installation path $target outside target directory $parent"
      case InvalidOutputPath(path) => s"Output file must be a markdown file (.md): $path"
      case IoError(detail) => s"I/O error: $detail"
      case InvalidAgent(name) =>
        s"Invalid agent: '$name'. Valid agents: ${Agent.all.map(_.toString.toLowerCase).mkString(", ")}"
    }
  }
}

final case class SyncOptions(
  skillName: Option[String] = None,
  from: Option[Agent] = None,
  to: Option[Agent] = None,
  allAgents: Boolean = false,
  yes: Boolean = false,
)

final case class InstallSourceInfo(
  source: String,
  sourceType: SkillSourceType,
  repoUrl: Option[String] = None,
  localRoot: Option[os.Path] = None,
)
