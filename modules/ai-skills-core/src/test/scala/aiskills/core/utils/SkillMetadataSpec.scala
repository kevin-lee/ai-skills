package aiskills.core.utils

import aiskills.core.{SkillSourceMetadata, SkillSourceType}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object SkillMetadataSpec extends Properties {

  override def tests: List[Test] = List(
    example("writes and reads metadata", testWriteAndRead),
    example("returns None when metadata is missing", testMissing),
    example("returns None for invalid JSON", testInvalidJson),
  )

  private def withTempDir[A](f: os.Path => A): A = {
    val tempDir = os.temp.dir(prefix = "aiskills-metadata-test-")
    try f(tempDir)
    finally os.remove.all(tempDir)
  }

  private def testWriteAndRead: Result =
    withTempDir { tempDir =>
      val payload = SkillSourceMetadata(
        source = "owner/repo",
        sourceType = SkillSourceType.Git,
        repoUrl = "https://github.com/owner/repo".some,
        subpath = "skills/demo".some,
        localPath = none[String],
        installedAt = "2026-01-01T00:00:00.000Z",
      )

      SkillMetadata.writeSkillMetadata(tempDir, payload)
      val read = SkillMetadata.readSkillMetadata(tempDir)

      read match {
        case Some(r) =>
          Result.all(
            List(
              r.source ==== payload.source,
              r.sourceType ==== payload.sourceType,
              r.repoUrl ==== payload.repoUrl,
              r.subpath ==== payload.subpath,
              r.installedAt ==== payload.installedAt,
            )
          )
        case None => Result.failure.log("Expected Some but got None")
      }
    }

  private def testMissing: Result =
    withTempDir { tempDir =>
      Result.assert(SkillMetadata.readSkillMetadata(tempDir).isEmpty)
    }

  private def testInvalidJson: Result =
    withTempDir { tempDir =>
      os.write(tempDir / SkillMetadata.SkillMetadataFile, "{not-json")
      Result.assert(SkillMetadata.readSkillMetadata(tempDir).isEmpty)
    }
}
