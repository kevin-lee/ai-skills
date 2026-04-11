package aiskills.cli.commands

import hedgehog.*
import hedgehog.runner.*

object SearchSpec extends Properties {

  override def tests: List[Test] = List(
    // formatInstalls
    example("formatInstalls: formats millions", testFormatMillions),
    example("formatInstalls: formats thousands", testFormatThousands),
    example("formatInstalls: formats small numbers", testFormatSmall),
    example("formatInstalls: formats zero", testFormatZero),
    example("formatInstalls: formats boundary at 1000", testFormatBoundaryThousand),
    example("formatInstalls: formats boundary at 1000000", testFormatBoundaryMillion),
    // collectSkillMds
    example("collectSkillMds: finds SKILL.md in direct subdirectories", testCollectDirect),
    example("collectSkillMds: finds SKILL.md in nested directories", testCollectNested),
    example("collectSkillMds: returns empty for non-existent directory", testCollectNonExistent),
    example("collectSkillMds: returns empty for empty directory", testCollectEmpty),
    example("collectSkillMds: ignores files (non-directories)", testCollectIgnoresFiles),
    // findSkillMd
    example("findSkillMd: finds by direct path", testFindDirectPath),
    example("findSkillMd: finds under skills/ directory", testFindUnderSkills),
    example("findSkillMd: finds by directory name recursively", testFindByDirName),
    example("findSkillMd: finds by YAML name field (case-insensitive)", testFindByYamlName),
    example("findSkillMd: returns None when skill not found", testFindNotFound),
    example("findSkillMd: prefers direct path over recursive match", testFindPrefersDirectPath),
  )

  // --- formatInstalls ---

  private def testFormatMillions: Result =
    Result.all(
      List(
        Search.formatInstalls(1_500_000L) ==== "1.5M installs",
        Search.formatInstalls(2_000_000L) ==== "2.0M installs",
        Search.formatInstalls(10_300_000L) ==== "10.3M installs",
      )
    )

  private def testFormatThousands: Result =
    Result.all(
      List(
        Search.formatInstalls(1_500L) ==== "1.5K installs",
        Search.formatInstalls(18_002L) ==== "18.0K installs",
        Search.formatInstalls(999_999L) ==== "1000.0K installs",
      )
    )

  private def testFormatSmall: Result =
    Result.all(
      List(
        Search.formatInstalls(1L) ==== "1 installs",
        Search.formatInstalls(42L) ==== "42 installs",
        Search.formatInstalls(999L) ==== "999 installs",
      )
    )

  private def testFormatZero: Result =
    Search.formatInstalls(0L) ==== "0 installs"

  private def testFormatBoundaryThousand: Result =
    Search.formatInstalls(1000L) ==== "1.0K installs"

  private def testFormatBoundaryMillion: Result =
    Search.formatInstalls(1_000_000L) ==== "1.0M installs"

  // --- collectSkillMds ---

  private def withTempDir[A](f: os.Path => A): A = {
    val dir = os.temp.dir(prefix = "search-test-")
    try f(dir)
    finally os.remove.all(dir)
  }

  private def createSkillMd(dir: os.Path, name: String, yamlName: String): os.Path = {
    val skillDir = dir / name
    os.makeDir.all(skillDir)
    val mdPath   = skillDir / "SKILL.md"
    os.write(
      mdPath,
      s"""---
         |name: $yamlName
         |description: A test skill
         |---
         |Content here
         |""".stripMargin,
    )
    mdPath
  }

  private def testCollectDirect: Result =
    withTempDir { dir =>
      val _       = createSkillMd(dir, "skill-a", "Skill A")
      val _       = createSkillMd(dir, "skill-b", "Skill B")
      val results = Search.collectSkillMds(dir)
      results.length ==== 2
    }

  private def testCollectNested: Result =
    withTempDir { dir =>
      val nested  = dir / "subdir"
      os.makeDir.all(nested)
      val _       = createSkillMd(nested, "nested-skill", "Nested Skill")
      val results = Search.collectSkillMds(dir)
      results.length ==== 1
    }

  private def testCollectNonExistent: Result =
    Search.collectSkillMds(os.pwd / "non-existent-dir-xyz") ==== Nil

  private def testCollectEmpty: Result =
    withTempDir { dir =>
      Search.collectSkillMds(dir) ==== Nil
    }

  private def testCollectIgnoresFiles: Result =
    withTempDir { dir =>
      os.write(dir / "README.md", "Not a skill")
      val _       = createSkillMd(dir, "real-skill", "Real Skill")
      val results = Search.collectSkillMds(dir)
      results.length ==== 1
    }

  // --- findSkillMd ---

  private def testFindDirectPath: Result =
    withTempDir { dir =>
      val md = createSkillMd(dir, "commit", "commit")
      Search.findSkillMd(dir, "commit") ==== Some(md)
    }

  private def testFindUnderSkills: Result =
    withTempDir { dir =>
      val skillsDir = dir / "skills"
      os.makeDir.all(skillsDir)
      val md        = createSkillMd(skillsDir, "commit", "commit")
      Search.findSkillMd(dir, "commit") ==== Some(md)
    }

  private def testFindByDirName: Result =
    withTempDir { dir =>
      val nested = dir / "some" / "path"
      os.makeDir.all(nested)
      val md     = createSkillMd(nested, "commit", "commit")
      Search.findSkillMd(dir, "commit") ==== Some(md)
    }

  private def testFindByYamlName: Result =
    withTempDir { dir =>
      // Directory name is kebab-case, but YAML name has spaces
      val md = createSkillMd(dir, "pdf-merge-split", "PDF Merge & Split")
      Search.findSkillMd(dir, "pdf merge & split") ==== Some(md)
    }

  private def testFindNotFound: Result =
    withTempDir { dir =>
      val _ = createSkillMd(dir, "commit", "commit")
      Search.findSkillMd(dir, "nonexistent") ==== None
    }

  private def testFindPrefersDirectPath: Result =
    withTempDir { dir =>
      // Create both a direct match and a nested match
      val directMd  = createSkillMd(dir, "commit", "commit")
      val nestedDir = dir / "other"
      os.makeDir.all(nestedDir)
      val _         = createSkillMd(nestedDir, "commit", "commit")
      // Direct path should be preferred
      Search.findSkillMd(dir, "commit") ==== Some(directMd)
    }
}
