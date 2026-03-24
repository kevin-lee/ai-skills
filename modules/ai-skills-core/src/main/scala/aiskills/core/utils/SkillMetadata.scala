package aiskills.core.utils

import aiskills.core.SkillSourceMetadata
import io.circe.parser.decode
import io.circe.syntax.*

import scala.util.Try

object SkillMetadata {

  val SkillMetadataFile: String = ".aiskills.json"

  /** Read skill source metadata from a skill directory. */
  def readSkillMetadata(skillDir: os.Path): Option[SkillSourceMetadata] = {
    val metadataPath = skillDir / SkillMetadataFile
    if !os.exists(metadataPath) then None
    else
      Try(os.read(metadataPath))
        .toOption
        .flatMap(raw => decode[SkillSourceMetadata](raw).toOption)
  }

  /** Write skill source metadata to a skill directory. */
  def writeSkillMetadata(skillDir: os.Path, metadata: SkillSourceMetadata): Unit = {
    val metadataPath = skillDir / SkillMetadataFile
    val payload      =
      if metadata.installedAt.isEmpty then metadata.copy(installedAt = isoNow())
      else
        metadata
    os.write.over(metadataPath, payload.asJson.spaces2)
  }
}
