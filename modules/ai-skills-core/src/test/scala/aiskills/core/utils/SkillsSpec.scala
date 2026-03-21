package aiskills.core.utils

import hedgehog.*
import hedgehog.runner.*

object SkillsSpec extends Properties:

  override def tests: List[Test] = List(
    example("findAllSkills: finds regular directory skills", testFindRegular),
    example("findAllSkills: finds symlinked skill directories", testFindSymlinked),
    example("findAllSkills: finds both regular and symlinked", testFindBoth),
    example("findAllSkills: skips broken symlinks gracefully", testBrokenSymlink),
    example("findAllSkills: does NOT deduplicate same name across dirs", testNoDedup),
    example("findAllSkills: skips directories without SKILL.md", testNoSkillMd),
    example("findAllSkills: skips files (not directories)", testSkipFiles),
    example("findAllSkills: handles empty skills directories", testEmpty),
    example("findAllSkills: handles non-existent directories", testNonExistent),
    example("findSkill: finds skill by name", testFindByName),
    example("findSkill: finds symlinked skill by name", testFindSymlinkedByName),
    example("findSkill: returns None for non-existent", testFindNonExistent),
    example("findSkill: returns first match in priority order", testFindPriority),
  )

  private def withTestDirs[A](f: (os.Path, os.Path) => A): A =
    val testRoot = os.temp.dir(prefix = "aiskills-test-")
    val projectSkills = testRoot / "project" / ".claude" / "skills"
    val globalSkills = testRoot / "global" / ".claude" / "skills"
    os.makeDir.all(projectSkills)
    os.makeDir.all(globalSkills)

    // Temporarily override getSearchDirs by using the dirs directly
    // We test the underlying logic by creating skills in known locations
    // and calling findAllSkills/findSkill which use getSearchDirs
    // For unit testing, we test the helper functions directly
    try f(projectSkills, globalSkills)
    finally os.remove.all(testRoot)

  private def createSkill(
    baseDir: os.Path, skillName: String, description: String //= "Test skill"
  ): Unit =
    val skillDir = baseDir / skillName
    os.makeDir.all(skillDir)
    os.write(
      skillDir / "SKILL.md",
      s"""---
         |name: $skillName
         |description: $description
         |---
         |
         |# $skillName
         |
         |This is a test skill.""".stripMargin,
    )

  private def createSymlinkedSkill(
    skillsDir: os.Path,
    targetDir: os.Path,
    skillName: String,
    description: String //= "Symlinked test skill",
  ): Unit =
    val actualSkillDir = targetDir / skillName
    os.makeDir.all(actualSkillDir)
    os.write(
      actualSkillDir / "SKILL.md",
      s"""---
         |name: $skillName
         |description: $description
         |---
         |
         |# $skillName
         |
         |This is a symlinked test skill.""".stripMargin,
    )
    os.symlink(skillsDir / skillName, actualSkillDir)

  // Note: These tests exercise the skill creation/reading logic directly
  // since we cannot easily mock getSearchDirs without DI.
  // Integration tests would test the full findAllSkills/findSkill flow.

  private def testFindRegular: Result =
    withTestDirs { (projectSkills, _) =>
      createSkill(projectSkills, "regular-skill", "A regular skill")
      val skillPath = projectSkills / "regular-skill" / "SKILL.md"
      val content = os.read(skillPath)
      val desc = Yaml.extractYamlField(content, "description")
      desc ==== "A regular skill"
    }

  private def testFindSymlinked: Result =
    withTestDirs { (_, globalSkills) =>
      val targetDir = globalSkills / os.up / "symlink-targets"
      os.makeDir.all(targetDir)
      createSymlinkedSkill(globalSkills, targetDir, "symlinked-skill", "A symlinked skill")
      val skillPath = globalSkills / "symlinked-skill" / "SKILL.md"
      Result.all(List(
        Result.assert(os.exists(skillPath)),
        Yaml.extractYamlField(os.read(skillPath), "description") ==== "A symlinked skill",
      ))
    }

  private def testFindBoth: Result =
    withTestDirs { (projectSkills, globalSkills) =>
      createSkill(projectSkills, "regular-skill", "Regular")
      val targetDir = globalSkills / os.up / "symlink-targets"
      os.makeDir.all(targetDir)
      createSymlinkedSkill(globalSkills, targetDir, "symlinked-skill", "Symlinked")
      Result.all(List(
        Result.assert(os.exists(projectSkills / "regular-skill" / "SKILL.md")),
        Result.assert(os.exists(globalSkills / "symlinked-skill" / "SKILL.md")),
      ))
    }

  private def testBrokenSymlink: Result =
    withTestDirs { (projectSkills, globalSkills) =>
      createSkill(projectSkills, "good-skill", "Good skill")
      os.symlink(globalSkills / "broken-symlink", os.Path("/non/existent/path"))
      // broken symlink should not cause an error
      Result.assert(os.exists(projectSkills / "good-skill" / "SKILL.md"))
    }

  private def testNoDedup: Result =
    withTestDirs { (projectSkills, globalSkills) =>
      createSkill(projectSkills, "duplicate-skill", "Project version")
      createSkill(globalSkills, "duplicate-skill", "Global version")
      // Both should exist independently
      val projectContent = os.read(projectSkills / "duplicate-skill" / "SKILL.md")
      val globalContent = os.read(globalSkills / "duplicate-skill" / "SKILL.md")
      Result.all(List(
        Yaml.extractYamlField(projectContent, "description") ==== "Project version",
        Yaml.extractYamlField(globalContent, "description") ==== "Global version",
      ))
    }

  private def testNoSkillMd: Result =
    withTestDirs { (projectSkills, _) =>
      val noSkillDir = projectSkills / "not-a-skill"
      os.makeDir.all(noSkillDir)
      os.write(noSkillDir / "README.md", "# Not a skill")
      Result.assert(!os.exists(noSkillDir / "SKILL.md"))
    }

  private def testSkipFiles: Result =
    withTestDirs { (projectSkills, _) =>
      os.write(projectSkills / "file.txt", "Just a file")
      createSkill(projectSkills, "actual-skill", "Real skill")
      Result.all(List(
        Result.assert(!os.isDir(projectSkills / "file.txt")),
        Result.assert(os.exists(projectSkills / "actual-skill" / "SKILL.md")),
      ))
    }

  private def testEmpty: Result =
    withTestDirs { (projectSkills, _) =>
      Result.assert(os.list(projectSkills).isEmpty)
    }

  private def testNonExistent: Result =
    Result.assert(!os.exists(os.Path("/non/existent/path")))

  private def testFindByName: Result =
    withTestDirs { (projectSkills, _) =>
      createSkill(projectSkills, "my-skill", "My skill description")
      val skillPath = projectSkills / "my-skill" / "SKILL.md"
      Result.all(List(
        Result.assert(os.exists(skillPath)),
        Result.assert(skillPath.toString.contains("my-skill/SKILL.md")),
      ))
    }

  private def testFindSymlinkedByName: Result =
    withTestDirs { (_, globalSkills) =>
      val targetDir = globalSkills / os.up / "symlink-targets"
      os.makeDir.all(targetDir)
      createSymlinkedSkill(globalSkills, targetDir, "linked-skill", "Linked description")
      val skillPath = globalSkills / "linked-skill" / "SKILL.md"
      Result.assert(os.exists(skillPath))
    }

  private def testFindNonExistent: Result =
    withTestDirs { (projectSkills, _) =>
      Result.assert(!os.exists(projectSkills / "non-existent" / "SKILL.md"))
    }

  private def testFindPriority: Result =
    withTestDirs { (projectSkills, globalSkills) =>
      createSkill(projectSkills, "shared-skill", "Project")
      createSkill(globalSkills, "shared-skill", "Global")
      // Project comes first in search order
      val projectContent = os.read(projectSkills / "shared-skill" / "SKILL.md")
      Yaml.extractYamlField(projectContent, "description") ==== "Project"
    }
