package aiskills.core.utils

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

import scala.util.Try

object SkillFilesSpec extends Properties {

  override def tests: List[Test] = List(
    property(
      "copyWithoutGit copies what os.copy copies, leaving out every entry named .git",
      testCopyMatchesOsCopyWithoutGit,
    ),
    example(
      "copyWithoutGit leaves out a repository's .git and keeps .gitignore, .github and .aiskills.json",
      testRepositoryGitLeftOut,
    ),
    example("copyWithoutGit leaves out a .git file and a nested repository's .git", testGitFileAndNestedGitLeftOut),
    example("copyWithoutGit copies the target of a symbolic link, as os.copy does", testSymlinkTargetCopied),
  )

  private def withTempDir[A](f: os.Path => A): A = {
    val tempDir = os.temp.dir(prefix = "aiskills-files-test-")
    try f(tempDir)
    finally os.remove.all(tempDir)
  }

  private def git(cwd: os.Path, args: List[String]): Unit = {
    val _ = os
      .proc(
        "git",
        "-c",
        "user.name=Files Test",
        "-c",
        "user.email=files-test@example.invalid",
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

  /** Every entry under `root` as its relative path and content, with `None` for a directory. */
  private def tree(root: os.Path): List[(String, Option[String])] =
    os.walk(root)
      .map(p => (p.relativeTo(root).toString, if os.isDir(p) then none[String] else os.read(p).some))
      .toList
      .sorted

  private val genSegment: Gen[String] = Gen.element1(".git", ".github", ".gitignore", "a", "b")

  private val genEntry: Gen[(List[String], String)] =
    for {
      segments <- Gen.list(genSegment, Range.linear(1, 4))
      content  <- Gen.string(Gen.alpha, Range.linear(0, 8))
    } yield (segments, content)

  private val genEntries: Gen[List[(List[String], String)]] = Gen.list(genEntry, Range.linear(0, 20))

  private def testCopyMatchesOsCopyWithoutGit: Property =
    for {
      entries <- genEntries.forAll
    } yield withTempDir { dir =>
      val src      = dir / "src"
      val expected = dir / "expected"
      val actual   = dir / "actual"
      os.makeDir(src)
      // An entry that clashes with an earlier file or folder is skipped.
      entries.foreach {
        case (segments, content) =>
          val _ = Try(os.write(segments.foldLeft(src)(_ / _), content, createFolders = true))
      }

      os.copy(src, expected)
      os.walk(expected).filter(_.last === ".git").foreach { p =>
        if os.exists(p) then os.remove.all(p) else ()
      }
      SkillFiles.copyWithoutGit(src, actual, replaceExisting = false)

      Result.all(
        List(
          tree(actual) ==== tree(expected),
          Result.assert(!os.walk(actual).exists(_.last === ".git")).log("Expected no entry named .git"),
        )
      )
    }

  private def testRepositoryGitLeftOut: Result =
    withTempDir { dir =>
      val src   = dir / "src"
      val dest  = dir / "dest"
      val files = List(
        os.RelPath("SKILL.md")                      -> "---\nname: src\ndescription: test\n---\nbody\n",
        os.RelPath(".gitignore")                    -> "*.log\n",
        os.RelPath(".github/workflows/ci.yml")      -> "name: ci\n",
        os.RelPath(SkillMetadata.SkillMetadataFile) -> "{}\n",
        os.RelPath("ref/notes.md")                  -> "notes\n",
      )
      files.foreach { case (rel, content) => os.write(src / rel, content, createFolders = true) }
      git(src, List("init", "--quiet"))
      git(src, List("add", "--all", "."))
      git(src, List("commit", "-m", "Skill"))

      SkillFiles.copyWithoutGit(src, dest, replaceExisting = false)
      val srcHash = SkillHash.directoryHash(src)

      Result.all(
        List(
          Result.assert(!os.exists(dest / ".git")).log("Expected no .git in the copy"),
          Result.assert(srcHash.isRight).log(s"source hash: $srcHash"),
          SkillHash.directoryHash(dest) ==== srcHash,
        ) ++ files.map { case (rel, content) => Try(os.read(dest / rel)).toOption ==== content.some }
      )
    }

  private def testGitFileAndNestedGitLeftOut: Result =
    withTempDir { dir =>
      val src  = dir / "src"
      val dest = dir / "dest"
      os.write(src / "SKILL.md", "---\nname: src\ndescription: test\n---\nbody\n", createFolders = true)
      os.write(src / ".git", "gitdir: /nonexistent/worktree\n")
      os.write(src / "vendor" / "lib" / ".git" / "HEAD", "ref: refs/heads/main\n", createFolders = true)
      os.write(src / "vendor" / "lib" / "README.md", "lib\n")

      SkillFiles.copyWithoutGit(src, dest, replaceExisting = false)

      Result.all(
        List(
          Result.assert(!os.exists(dest / ".git")).log("Expected no top-level .git file in the copy"),
          Result.assert(!os.exists(dest / "vendor" / "lib" / ".git")).log("Expected no nested .git in the copy"),
          Try(os.read(dest / "vendor" / "lib" / "README.md")).toOption ==== "lib\n".some,
        )
      )
    }

  private def testSymlinkTargetCopied: Result =
    withTempDir { dir =>
      val src  = dir / "src"
      val dest = dir / "dest"
      os.write(src / "SKILL.md", "---\nname: src\ndescription: test\n---\nbody\n", createFolders = true)
      os.symlink(src / "link.md", src / "SKILL.md")

      SkillFiles.copyWithoutGit(src, dest, replaceExisting = false)

      Result.all(
        List(
          Result.assert(!os.isLink(dest / "link.md")).log("Expected link.md to be copied as a file"),
          Try(os.read(dest / "link.md")).toOption ==== os.read(src / "SKILL.md").some,
        )
      )
    }

}
