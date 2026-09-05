package aiskills.core

import aiskills.core.internal.GitHubOwnerRepoValidator
import cats.*
import cats.derived.*
import cats.syntax.all.*
import io.circe.{Codec, Decoder, Encoder}
import refined4s.*
import refined4s.modules.cats.derivation.*
import refined4s.modules.circe.derivation.*

given Eq[os.Path]   = Eq.fromUniversalEquals
given Show[os.Path] = Show.fromToString

/** How an agent's global config dir can be relocated via its own env var.
  *   - ConfigDir: the env var value is the config dir itself; skills live at `<value>/skills`.
  *   - HomeRoot: the env var value replaces the home root; skills live at `<value>/<globalDirName>/skills`.
  */
enum GlobalDirOverride derives Eq, Show {
  case ConfigDir(envVarName: String)
  case HomeRoot(envVarName: String)
  case NoOverride
}

/** Resolved global config base dir (without the `skills` segment).
  * `envVar` is Some(varName) only when an override was actually applied.
  */
final case class GlobalDirResolution(
  configBase: os.Path,
  envVar: Option[String],
) derives Eq,
      Show

enum Agent(
  val projectDirName: String,
  val globalDirName: String,
  val globalDirOverride: GlobalDirOverride,
) derives Eq,
      Show {
  case Universal extends Agent(".agents", ".agents", GlobalDirOverride.NoOverride)
  case Claude extends Agent(".claude", ".claude", GlobalDirOverride.ConfigDir("CLAUDE_CONFIG_DIR"))
  case Cursor extends Agent(".cursor", ".cursor", GlobalDirOverride.NoOverride)
  case Codex extends Agent(".codex", ".codex", GlobalDirOverride.ConfigDir("CODEX_HOME"))
  case Gemini extends Agent(".gemini", ".gemini", GlobalDirOverride.HomeRoot("GEMINI_CLI_HOME"))
  case Windsurf extends Agent(".windsurf", ".codeium/windsurf", GlobalDirOverride.NoOverride)
  case Copilot extends Agent(".github", ".copilot", GlobalDirOverride.ConfigDir("COPILOT_HOME"))
}
object Agent {

  val all: List[Agent] = Agent.values.toList

  val allNonUniversal: List[Agent] = all.filterNot(_ === Agent.Universal)

  def fromString(s: String): Either[String, Agent] =
    all
      .find(_.toString.equalsIgnoreCase(s))
      .toRight(s"Invalid Agent: $s. Valid agents: ${all.map(_.toString.toLowerCase).mkString(", ")}")

  def needsAgentsMd(agent: Agent): Boolean =
    agent match {
      case Agent.Universal | Agent.Codex => true
      case Agent.Claude | Agent.Cursor | Agent.Gemini | Agent.Windsurf | Agent.Copilot => false
    }

  given Encoder[Agent] = Encoder.encodeString.contramap(_.toString.toLowerCase)

  given Decoder[Agent] = Decoder.decodeString.emap(fromString)
}

enum SkillLocation derives Eq, Show {
  case Project, Global
}
object SkillLocation {

  def fromString(s: String): Either[String, SkillLocation] = s match {
    case "project" => SkillLocation.Project.asRight
    case "global" => SkillLocation.Global.asRight
    case other => s"Invalid SkillLocation: $other".asLeft
  }

  given ordering: Ordering[SkillLocation] = Ordering.by {
    case SkillLocation.Project => 1
    case SkillLocation.Global => 0
  }

  given Encoder[SkillLocation] = Encoder.encodeString.contramap {
    case SkillLocation.Project => "project"
    case SkillLocation.Global => "global"
  }

  given Decoder[SkillLocation] = Decoder.decodeString.emap(fromString)
}

final case class Skill(
  name: String,
  description: String,
  location: SkillLocation,
  agent: Agent,
  path: os.Path,
) derives Eq,
      Show

final case class SkillLocationInfo(
  path: os.Path,
  baseDir: os.Path,
  source: os.Path,
  agent: Agent,
  location: SkillLocation,
) derives Eq,
      Show

final case class InstallOptions(
  branch: Option[GitBranch],
  locations: Set[SkillLocation],
  agent: Option[List[Agent]],
  yes: Boolean,
) derives Eq,
      Show

final case class ReadOptions(
  locations: Set[SkillLocation],
  agent: Option[List[Agent]],
) derives Eq,
      Show

final case class ListOptions(
  locations: Set[SkillLocation],
  agent: Option[List[Agent]],
) derives Eq,
      Show

final case class SkillMetadata(
  name: String,
  description: String,
  context: Option[String],
) derives Eq,
      Show

enum SkillSourceType derives Eq, Show {
  case Git, Local
}
object SkillSourceType {

  given Encoder[SkillSourceType] = Encoder.encodeString.contramap {
    case SkillSourceType.Git => "git"
    case SkillSourceType.Local => "local"
  }

  given Decoder[SkillSourceType] = Decoder.decodeString.emap {
    case "git" => SkillSourceType.Git.asRight
    case "github" => SkillSourceType.Git.asRight // legacy alias
    case "local" => SkillSourceType.Local.asRight
    case other => s"Invalid SkillSourceType: $other".asLeft
  }
}

/** Any git clone source: an https/ssh/git URL or a local .git path. */
type RepoUrl = RepoUrl.Type
object RepoUrl extends Newtype[String], CatsEqShow[String], CirceNewtypeCodec[String]

/** An explicitly selected Git branch. Validated at the Git operation boundary. */
type GitBranch = GitBranch.Type
object GitBranch extends Newtype[String], CatsEqShow[String], CirceNewtypeCodec[String]

/** A GitHub repo identity in `owner/repo` form. */
type GitHubOwnerRepo = GitHubOwnerRepo.Type
object GitHubOwnerRepo extends InlinedRefined[String], CatsEqShow[String] {

  override inline def inlinedExpectedValue: String =
    """in the form owner/repo with non-empty owner and repo (e.g. "kevin-lee/ai-dumping-ground")"""

  override inline def inlinedPredicate(inline a: String): Boolean =
    ${ GitHubOwnerRepoValidator.isValidExpr('a) }

  override def invalidReason(a: String): String =
    expectedMessage("""in the form owner/repo with non-empty owner and repo (e.g. "kevin-lee/ai-dumping-ground")""")

  override def predicate(a: String): Boolean = GitHubOwnerRepoValidator.isValid(a)

}

/** How a Git repository was successfully accessed. */
enum GitAuthMethod derives Eq, Show {
  case Anonymous, Ssh, Gh, CredentialHelper, Interactive
}
object GitAuthMethod {

  def render(method: GitAuthMethod): String = method match {
    case GitAuthMethod.Anonymous => "anonymous"
    case GitAuthMethod.Ssh => "ssh"
    case GitAuthMethod.Gh => "gh"
    case GitAuthMethod.CredentialHelper => "credential-helper"
    case GitAuthMethod.Interactive => "interactive"
  }

  def fromString(s: String): Either[String, GitAuthMethod] = s match {
    case "anonymous" => GitAuthMethod.Anonymous.asRight
    case "ssh" => GitAuthMethod.Ssh.asRight
    case "gh" => GitAuthMethod.Gh.asRight
    case "credential-helper" => GitAuthMethod.CredentialHelper.asRight
    case "interactive" => GitAuthMethod.Interactive.asRight
    case other => s"Invalid GitAuthMethod: $other".asLeft
  }

  given Encoder[GitAuthMethod] = Encoder.encodeString.contramap(render)

  given Decoder[GitAuthMethod] = Decoder.decodeString.emap(fromString)
}

final case class SkillSourceMetadata private (
  name: Option[String], // For backward compat: existing .aiskills.json without name
  source: String,
  sourceType: SkillSourceType,
  repoUrl: Option[RepoUrl],
  branch: Option[GitBranch],
  authMethod: Option[GitAuthMethod], // None means "no recorded method": run the full fallback chain
  subpath: Option[String], // Canonical: None means the skill is at the repo root
  localPath: Option[String],
  installedAt: String,
) derives Eq,
      Show
object SkillSourceMetadata {

  extension (skillSourceMetadata: SkillSourceMetadata) {

    def withName(newName: String): SkillSourceMetadata = skillSourceMetadata.copy(name = newName.some)

    def withInstalledAt(at: String): SkillSourceMetadata = skillSourceMetadata.copy(installedAt = at)

    def withRepoUrl(url: Option[RepoUrl]): SkillSourceMetadata = skillSourceMetadata.copy(repoUrl = url)

    def withBranch(branch: Option[GitBranch]): SkillSourceMetadata = skillSourceMetadata.copy(branch = branch)

    def withAuthMethod(method: Option[GitAuthMethod]): SkillSourceMetadata =
      skillSourceMetadata.copy(authMethod = method)
  }

  /** Canonicalize the repo-root subpath. `""`, `"."`, and whitespace-only all mean "repo root",
    * which is represented as `None`.
    */
  def normalizeSubpath(subpath: Option[String]): Option[String] =
    subpath.map(_.trim).filter(s => s.nonEmpty && s =!= ".")

  def apply(
    name: Option[String] = none[String], // scalafix:ok DisableSyntax.defaultArgs
    source: String,
    sourceType: SkillSourceType,
    repoUrl: Option[RepoUrl],
    branch: Option[GitBranch],
    authMethod: Option[GitAuthMethod],
    subpath: Option[String],
    localPath: Option[String],
    installedAt: String,
  ): SkillSourceMetadata =
    new SkillSourceMetadata(
      name,
      source,
      sourceType,
      repoUrl,
      branch,
      authMethod,
      normalizeSubpath(subpath),
      localPath,
      installedAt,
    )

  private val derivedCodec: Codec.AsObject[SkillSourceMetadata] = Codec.AsObject.derived

  given Codec.AsObject[SkillSourceMetadata] =
    Codec
      .AsObject
      .from(
        derivedCodec.map(m =>
          SkillSourceMetadata(
            m.name,
            m.source,
            m.sourceType,
            m.repoUrl,
            m.branch,
            m.authMethod,
            m.subpath,
            m.localPath,
            m.installedAt,
          )
        ),
        derivedCodec,
      )
}

enum AiSkillsError derives Eq, Show {
  case SkillNotFound(name: String) extends AiSkillsError
  case GitCloneError(url: String, detail: String) extends AiSkillsError
  case MetadataParseError(path: os.Path, detail: String) extends AiSkillsError
  case InvalidFrontmatter(path: os.Path) extends AiSkillsError
  case InvalidSource(source: String) extends AiSkillsError
  case PathTraversalError(target: os.Path, parent: os.Path) extends AiSkillsError
  case InvalidOutputPath(path: String) extends AiSkillsError
  case IoError(detail: String) extends AiSkillsError
  case InvalidAgent(name: String) extends AiSkillsError
}
object AiSkillsError {

  def skillNotFound(name: String): AiSkillsError                       = AiSkillsError.SkillNotFound(name)
  def gitCloneError(url: String, detail: String): AiSkillsError        = AiSkillsError.GitCloneError(url, detail)
  def metadataParseError(path: os.Path, detail: String): AiSkillsError = AiSkillsError.MetadataParseError(path, detail)
  def invalidFrontmatter(path: os.Path): AiSkillsError                 = AiSkillsError.InvalidFrontmatter(path)
  def invalidSource(source: String): AiSkillsError                     = AiSkillsError.InvalidSource(source)
  def pathTraversalError(
    target: os.Path,
    parent: os.Path
  ): AiSkillsError                                   = AiSkillsError.PathTraversalError(target, parent)
  def invalidOutputPath(path: String): AiSkillsError = AiSkillsError.InvalidOutputPath(path)
  def ioError(detail: String): AiSkillsError         = AiSkillsError.IoError(detail)
  def invalidAgent(name: String): AiSkillsError      = AiSkillsError.InvalidAgent(name)

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

final case class RemoveOptions(
  locations: Set[SkillLocation],
  agent: Option[List[Agent]],
  yes: Boolean,
) derives Eq,
      Show

final case class SyncOptions(
  skillNames: List[String],
  from: Option[(SkillLocation, Agent)],
  to: Option[List[Agent]],
  targetLocations: Set[SkillLocation],
  yes: Boolean,
) derives Eq,
      Show

final case class InstallSourceInfo(
  source: String,
  sourceType: SkillSourceType,
  repoUrl: Option[RepoUrl],
  branch: Option[GitBranch],
  authMethod: Option[GitAuthMethod],
  localRoot: Option[os.Path],
) derives Eq,
      Show
