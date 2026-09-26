package aiskills.core.utils

import aiskills.core.{ContentHash, GitCommitHash}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object SkillHashSpec extends Properties {

  override def tests: List[Test] = List(
    example("directoryHash matches the tree Git records for the same files", testDirectoryHashMatchesGit),
    example(
      "directoryHash ignores the top-level .aiskills.json and operating system files at any depth",
      testDirectoryHashIgnoresExcludedFiles,
    ),
    example("directoryHash changes when a file is edited, added or renamed", testDirectoryHashChanges),
    example("directoryHash treats CRLF and LF line endings as the same content", testDirectoryHashLineEndings),
    example("gitTreeHash of a subfolder equals directoryHash of that folder", testGitTreeHashSubfolder),
    example("gitTreeHash of the repository root equals directoryHash of the checkout", testGitTreeHashRoot),
    example("a commit to another path keeps the skill's tree hash", testCommitToOtherPath),
    example("gitCommit returns the checked-out commit", testGitCommit),
    example("directoryHash rejects a missing directory", testMissingDirectory),
    example("gitTreeHash fails for a missing subpath", testMissingSubpath),
    example("source hashes are unavailable for a folder with a symbolic link", testSymlink),
  )

  private def withTempDir[A](f: os.Path => A): A = {
    val tempDir = os.temp.dir(prefix = "aiskills-hash-test-")
    try f(tempDir)
    finally os.remove.all(tempDir)
  }

  private def git(cwd: os.Path, args: List[String]): Unit = {
    val _ = os
      .proc(
        "git",
        "-c",
        "user.name=Hash Test",
        "-c",
        "user.email=hash-test@example.invalid",
        "-c",
        "commit.gpgsign=false",
        "-c",
        "core.hooksPath=/dev/null",
        "-c",
        "core.autocrlf=false",
        args,
      )
      .call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe)
  }

  private def commitAll(repo: os.Path, message: String): Unit = {
    git(repo, List("add", "--all", "."))
    git(repo, List("commit", "-m", message))
  }

  private def initRepo(repo: os.Path): Unit = {
    git(repo, List("init", "--quiet"))
    commitAll(repo, "Initial skills")
  }

  private def revParse(cwd: os.Path, rev: String): String =
    os.proc("git", "rev-parse", rev).call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe).out.text().trim

  private def writeSkill(dir: os.Path, body: String): Unit = {
    os.makeDir.all(dir)
    os.write.over(dir / "SKILL.md", s"---\nname: ${dir.last}\ndescription: test\n---\n$body\n")
  }

  private def writeRepoWithTwoSkills(repo: os.Path): Unit = {
    writeSkill(repo / "skills" / "a", "skill a")
    writeSkill(repo / "skills" / "b", "skill b")
    os.write(repo / "README.md", "repo\n", createFolders = true)
    initRepo(repo)
  }

  private def testDirectoryHashMatchesGit: Result =
    withTempDir { dir =>
      val skill = dir / "skill"
      writeSkill(skill, "body")
      os.write(skill / "sub" / "ref.md", "reference\n", createFolders = true)
      initRepo(skill)
      SkillHash.directoryHash(skill) ==== Right(ContentHash(revParse(skill, "HEAD^{tree}")))
    }

  private def testDirectoryHashIgnoresExcludedFiles: Result =
    withTempDir { dir =>
      val skill  = dir / "skill"
      writeSkill(skill, "body")
      os.makeDir.all(skill / "sub")
      val before = SkillHash.directoryHash(skill)
      os.write(skill / SkillMetadata.SkillMetadataFile, "{}")
      os.write(skill / ".DS_Store", "finder")
      os.write(skill / "sub" / ".DS_Store", "finder")
      os.write(skill / "Thumbs.db", "thumbs")
      os.write(skill / "sub" / "desktop.ini", "ini")
      val after  = SkillHash.directoryHash(skill)
      Result.all(
        List(
          Result.assert(before.isRight).log(s"before: $before"),
          after ==== before,
        )
      )
    }

  private def testDirectoryHashChanges: Result =
    withTempDir { dir =>
      val base     = dir / "base"
      writeSkill(base, "body")
      os.write(base / "ref.md", "reference\n")
      val edited   = dir / "edited"
      val added    = dir / "added"
      val renamed  = dir / "renamed"
      List(edited, added, renamed).foreach(os.copy(base, _))
      os.write.append(edited / "SKILL.md", "more\n")
      os.write(added / "extra.md", "extra\n")
      os.move(renamed / "ref.md", renamed / "reference.md")
      val baseHash = SkillHash.directoryHash(base)
      Result.all(
        Result.assert(baseHash.isRight).log(s"base: $baseHash") ::
          List(edited, added, renamed).map { changed =>
            val changedHash = SkillHash.directoryHash(changed)
            Result
              .assert(changedHash.isRight && changedHash =!= baseHash)
              .log(s"${changed.last}: $changedHash, base: $baseHash")
          }
      )
    }

  private def testDirectoryHashLineEndings: Result =
    withTempDir { dir =>
      val lf     = dir / "lf"
      val crlf   = dir / "crlf"
      os.makeDir.all(lf)
      os.makeDir.all(crlf)
      os.write(lf / "SKILL.md", "---\nname: demo\ndescription: test\n---\nbody\n")
      os.write(crlf / "SKILL.md", "---\r\nname: demo\r\ndescription: test\r\n---\r\nbody\r\n")
      val lfHash = SkillHash.directoryHash(lf)
      Result.all(
        List(
          Result.assert(lfHash.isRight).log(s"lf: $lfHash"),
          SkillHash.directoryHash(crlf) ==== lfHash,
        )
      )
    }

  private def testGitTreeHashSubfolder: Result =
    withTempDir { dir =>
      val repo     = dir / "repo"
      writeRepoWithTwoSkills(repo)
      val treeHash = SkillHash.gitTreeHash(repo, "skills/a".some)
      Result.all(
        List(
          Result.assert(treeHash.isRight).log(s"tree: $treeHash"),
          treeHash ==== SkillHash.directoryHash(repo / "skills" / "a"),
        )
      )
    }

  private def testGitTreeHashRoot: Result =
    withTempDir { dir =>
      val repo     = dir / "repo"
      writeRepoWithTwoSkills(repo)
      val treeHash = SkillHash.gitTreeHash(repo, none[String])
      Result.all(
        List(
          Result.assert(treeHash.isRight).log(s"tree: $treeHash"),
          treeHash ==== SkillHash.directoryHash(repo),
        )
      )
    }

  private def testCommitToOtherPath: Result =
    withTempDir { dir =>
      val repo         = dir / "repo"
      writeRepoWithTwoSkills(repo)
      val treeBefore   = SkillHash.gitTreeHash(repo, "skills/a".some)
      val commitBefore = SkillHash.gitCommit(repo)
      os.write.append(repo / "skills" / "b" / "SKILL.md", "changed\n")
      commitAll(repo, "Change skill b")
      val commitAfter  = SkillHash.gitCommit(repo)
      Result.all(
        List(
          Result.assert(treeBefore.isRight).log(s"tree: $treeBefore"),
          SkillHash.gitTreeHash(repo, "skills/a".some) ==== treeBefore,
          Result
            .assert(commitBefore.isRight && commitAfter.isRight && commitAfter =!= commitBefore)
            .log(s"before: $commitBefore, after: $commitAfter"),
        )
      )
    }

  private def testGitCommit: Result =
    withTempDir { dir =>
      val repo = dir / "repo"
      writeRepoWithTwoSkills(repo)
      SkillHash.gitCommit(repo) ==== Right(GitCommitHash(revParse(repo, "HEAD")))
    }

  private def testMissingDirectory: Result =
    withTempDir { dir =>
      val missing = dir / "missing"
      SkillHash.directoryHash(missing) ==== Left(SkillHashError.NotADirectory(missing))
    }

  private def testMissingSubpath: Result =
    withTempDir { dir =>
      val repo   = dir / "repo"
      writeRepoWithTwoSkills(repo)
      val result = SkillHash.gitTreeHash(repo, "skills/missing".some)
      Result.assert(result.isLeft).log(s"result: $result")
    }

  private def testSymlink: Result =
    withTempDir { dir =>
      val skill = dir / "skill"
      writeSkill(skill, "body")
      os.symlink(skill / "link.md", skill / "SKILL.md")

      val repo      = dir / "repo"
      writeSkill(repo / "skills" / "linked", "body")
      os.symlink(repo / "skills" / "linked" / "link.md", os.RelPath("SKILL.md"))
      initRepo(repo)
      val gitResult = SkillHash.sourceGitTreeHash(repo, "skills/linked".some)

      Result.all(
        List(
          SkillHash.sourceDirectoryHash(skill) ==== Left(SkillHashError.ContainsSymlink(skill)),
          Result.assert(gitResult.isLeft).log(s"git result: $gitResult"),
        )
      )
    }

}
