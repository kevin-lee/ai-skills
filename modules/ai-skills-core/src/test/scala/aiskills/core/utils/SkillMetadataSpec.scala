package aiskills.core.utils

import aiskills.core.{GitAuthMethod, SkillSourceMetadata, SkillSourceType}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object SkillMetadataSpec extends Properties {

  override def tests: List[Test] = List(
    example("writes and reads metadata", testWriteAndRead),
    example("writes and reads metadata with name", testWriteAndReadWithName),
    example("reads legacy metadata without name as None", testLegacyMetadataWithoutName),
    example("returns None when metadata is missing", testMissing),
    example("returns None for invalid JSON", testInvalidJson),
    example("normalizes \".\" subpath to None on construction", testNormalizesDotSubpathOnConstruction),
    example(
      "normalizes empty and whitespace-only subpath to None on construction",
      testNormalizesEmptySubpathOnConstruction,
    ),
    example("preserves a real subpath on construction", testPreservesRealSubpathOnConstruction),
    example("reads legacy \".\" subpath as None", testLegacyDotSubpath),
    example("reads legacy empty subpath as None", testLegacyEmptySubpath),
    example("writes root subpath as JSON null", testWritesRootSubpathAsNull),
    example("writes and reads authMethod", testWriteAndReadAuthMethod),
    example("reads legacy metadata without authMethod as None", testLegacyMetadataWithoutAuthMethod),
    example("GitAuthMethod: render and fromString round-trip", testGitAuthMethodRoundTrip),
    example("GitAuthMethod: fromString rejects an unknown value", testGitAuthMethodUnknown),
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
        authMethod = none[GitAuthMethod],
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

  private def testWriteAndReadWithName: Result =
    withTempDir { tempDir =>
      val payload = SkillSourceMetadata(
        name = "renamed-skill".some,
        source = "owner/repo",
        sourceType = SkillSourceType.Git,
        repoUrl = "https://github.com/owner/repo".some,
        authMethod = none[GitAuthMethod],
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
              r.name ==== "renamed-skill".some,
              r.source ==== payload.source,
              r.sourceType ==== payload.sourceType,
            )
          )
        case None => Result.failure.log("Expected Some but got None")
      }
    }

  private def testLegacyMetadataWithoutName: Result =
    withTempDir { tempDir =>
      // Write JSON without "name" key, simulating legacy metadata
      val legacyJson =
        """{
          |  "source" : "owner/repo",
          |  "sourceType" : "git",
          |  "repoUrl" : "https://github.com/owner/repo",
          |  "subpath" : "skills/demo",
          |  "localPath" : null,
          |  "installedAt" : "2026-01-01T00:00:00.000Z"
          |}""".stripMargin
      os.write(tempDir / SkillMetadata.SkillMetadataFile, legacyJson)
      val read       = SkillMetadata.readSkillMetadata(tempDir)

      read match {
        case Some(r) =>
          Result.all(
            List(
              r.name ==== none[String],
              r.source ==== "owner/repo",
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

  private def metadataWithSubpath(subpath: Option[String]): SkillSourceMetadata =
    SkillSourceMetadata(
      source = "owner/repo",
      sourceType = SkillSourceType.Git,
      repoUrl = "https://github.com/owner/repo".some,
      authMethod = none[GitAuthMethod],
      subpath = subpath,
      localPath = none[String],
      installedAt = "2026-01-01T00:00:00.000Z",
    )

  private def testNormalizesDotSubpathOnConstruction: Result =
    metadataWithSubpath(".".some).subpath ==== none[String]

  private def testNormalizesEmptySubpathOnConstruction: Result =
    Result.all(
      List(
        metadataWithSubpath("".some).subpath ==== none[String],
        metadataWithSubpath("   ".some).subpath ==== none[String],
      )
    )

  private def testPreservesRealSubpathOnConstruction: Result =
    metadataWithSubpath("skills/demo".some).subpath ==== "skills/demo".some

  private def legacyJsonWithSubpath(subpath: String): String =
    s"""{
       |  "source" : "owner/repo",
       |  "sourceType" : "git",
       |  "repoUrl" : "https://github.com/owner/repo",
       |  "subpath" : "$subpath",
       |  "localPath" : null,
       |  "installedAt" : "2026-01-01T00:00:00.000Z"
       |}""".stripMargin

  private def testLegacyDotSubpath: Result =
    withTempDir { tempDir =>
      // Write JSON with "." subpath literally, simulating records written by earlier versions
      os.write(tempDir / SkillMetadata.SkillMetadataFile, legacyJsonWithSubpath("."))
      val read = SkillMetadata.readSkillMetadata(tempDir)

      read match {
        case Some(r) => r.subpath ==== none[String]
        case None => Result.failure.log("Expected Some but got None")
      }
    }

  private def testLegacyEmptySubpath: Result =
    withTempDir { tempDir =>
      // Write JSON with "" subpath literally, simulating records written by earlier versions
      os.write(tempDir / SkillMetadata.SkillMetadataFile, legacyJsonWithSubpath(""))
      val read = SkillMetadata.readSkillMetadata(tempDir)

      read match {
        case Some(r) => r.subpath ==== none[String]
        case None => Result.failure.log("Expected Some but got None")
      }
    }

  private def testWritesRootSubpathAsNull: Result =
    withTempDir { tempDir =>
      SkillMetadata.writeSkillMetadata(tempDir, metadataWithSubpath(".".some))
      val raw = os.read(tempDir / SkillMetadata.SkillMetadataFile)
      Result.assert(raw.contains("\"subpath\" : null")).log(s"raw was: $raw")
    }

  private def testWriteAndReadAuthMethod: Result =
    withTempDir { tempDir =>
      val payload = SkillSourceMetadata(
        source = "owner/repo",
        sourceType = SkillSourceType.Git,
        repoUrl = "https://github.com/owner/repo".some,
        authMethod = GitAuthMethod.Gh.some,
        subpath = "skills/demo".some,
        localPath = none[String],
        installedAt = "2026-01-01T00:00:00.000Z",
      )

      SkillMetadata.writeSkillMetadata(tempDir, payload)
      val read = SkillMetadata.readSkillMetadata(tempDir)

      read match {
        case Some(r) => r.authMethod ==== GitAuthMethod.Gh.some
        case None => Result.failure.log("Expected Some but got None")
      }
    }

  private def testLegacyMetadataWithoutAuthMethod: Result =
    withTempDir { tempDir =>
      // Write JSON without "authMethod" key, simulating metadata written before the fallback chain
      val legacyJson =
        """{
          |  "source" : "owner/repo",
          |  "sourceType" : "git",
          |  "repoUrl" : "git@github.com:owner/repo.git",
          |  "subpath" : "skills/demo",
          |  "localPath" : null,
          |  "installedAt" : "2026-01-01T00:00:00.000Z"
          |}""".stripMargin
      os.write(tempDir / SkillMetadata.SkillMetadataFile, legacyJson)
      val read       = SkillMetadata.readSkillMetadata(tempDir)

      read match {
        case Some(r) =>
          Result.all(
            List(
              r.authMethod ==== none[GitAuthMethod],
              r.repoUrl ==== "git@github.com:owner/repo.git".some,
            )
          )
        case None => Result.failure.log("Expected Some but got None")
      }
    }

  private def testGitAuthMethodRoundTrip: Result =
    Result.all(
      GitAuthMethod
        .values
        .toList
        .map(method => GitAuthMethod.fromString(GitAuthMethod.render(method)) ==== method.asRight[String])
    )

  private def testGitAuthMethodUnknown: Result =
    GitAuthMethod.fromString("carrier-pigeon") ==== "Invalid GitAuthMethod: carrier-pigeon".asLeft[GitAuthMethod]
}
