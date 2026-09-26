package aiskills.core.utils

import aiskills.core.SkillSourceMetadata
import cats.syntax.all.*
import io.circe.parser.decode
import io.circe.syntax.*

import scala.util.Try

object SkillMetadata {

  val SkillMetadataFile: String = ".aiskills.json"

  /** Read skill source metadata from a skill directory. */
  def readSkillMetadata(skillDir: os.Path): Option[SkillSourceMetadata] = {
    val metadataPath = skillDir / SkillMetadataFile
    if !os.exists(metadataPath) then none[SkillSourceMetadata]
    else
      Try(os.read(metadataPath))
        .toOption
        .flatMap(raw => decode[SkillSourceMetadata](raw).toOption)
  }

  /** Write skill source metadata to a skill directory. The metadata is written as given and `installedHash` is
    * never recomputed, so recording `checkedAt` cannot absorb local edits.
    */
  def writeSkillMetadata(skillDir: os.Path, metadata: SkillSourceMetadata): Unit = {
    val metadataPath = skillDir / SkillMetadataFile
    val payload      =
      if metadata.installedAt.isEmpty then metadata.withInstalledAt(isoNow())
      else
        metadata
    os.write.over(metadataPath, payload.asJson.spaces2)
  }

  /** Record the installed state: hash the skill's files as they are now and write the metadata with that
    * `installedHash`. Call it only right after aiskills has written the skill's files.
    */
  def writeInstalledSkillMetadata(skillDir: os.Path, metadata: SkillSourceMetadata): Unit =
    writeSkillMetadata(skillDir, metadata.withInstalledHash(SkillHash.directoryHash(skillDir).toOption))
}
