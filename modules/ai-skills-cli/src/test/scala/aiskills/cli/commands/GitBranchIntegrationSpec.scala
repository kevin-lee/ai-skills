package aiskills.cli.commands

import aiskills.core.{GitAuthMethod, GitBranch, RepoUrl}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object GitBranchIntegrationSpec extends Properties {
  override def tests: List[Test] = List(
    example("clones the selected slash-containing branch and verifies its checkout", testSelectedBranch),
    example("omitted branch follows the remote default", testDefaultBranch),
    example("missing branch is distinguished from clone failure", testMissingBranch),
    example("tag-only names are not accepted as branches", testTagOnly),
    example("a branch wins over a same-named tag", testBranchAndTag),
    example("inaccessible repository is an ordinary failure", testInaccessibleRepo),
  )

  private val feature = GitBranch("feature/New-Skill")

  private def git(repo: os.Path, args: List[String]): Unit = {
    val _ = os
      .proc(
        "git",
        "-c",
        "user.name=Branch Test",
        "-c",
        "user.email=branch-test@example.invalid",
        "-c",
        "commit.gpgsign=false",
        "-c",
        "tag.gpgSign=false",
        "-c",
        "core.hooksPath=/dev/null",
        args
      )
      .call(cwd = repo, stdout = os.Pipe, stderr = os.Pipe)
  }

  private def withRepo(branches: List[GitBranch])(test: (os.Path, os.Path) => Result): Result = {
    val temp = os.temp.dir(prefix = "aiskills-branch-test-")
    val repo = temp / "remote"
    try {
      os.makeDir(repo)
      git(repo, List("init", "--initial-branch=trunk"))
      os.write(repo / "SKILL.md", "---\nname: demo\ndescription: default\n---\ndefault content\n")
      git(repo, List("add", "SKILL.md"))
      git(repo, List("commit", "-m", "Default skill"))
      git(repo, List("tag", "tag-only"))
      git(repo, List("tag", "feature/shared"))
      branches.foreach { branch =>
        git(repo, List("switch", "-c", branch.value, "trunk"))
        os.write.over(repo / "SKILL.md", s"---\nname: demo\ndescription: branch\n---\n${branch.value}\n")
        git(repo, List("add", "SKILL.md"))
        git(repo, List("commit", "-m", "Branch skill"))
      }
      git(repo, List("switch", "trunk"))
      test(temp, repo)
    } finally {
      os.remove.all(temp)
    }
  }

  private def clone(repo: os.Path, target: os.Path, branch: Option[GitBranch]) = {
    GitClone.cloneWithFallback(
      RepoUrl(s"file://$repo"),
      target,
      branch,
      none[GitAuthMethod],
      GitClone.Interactivity.NotAllowed
    )
  }

  private def testSelectedBranch: Result = withRepo(List(feature)) { (temp, repo) =>
    val target = temp / "clone"
    val result = clone(repo, target, feature.some)
    result match {
      case Left(error) => Result.failure.log(error.toString)
      case Right(_) =>
        val ref = os.proc("git", "symbolic-ref", "--quiet", "HEAD").call(cwd = target, stdout = os.Pipe).out.text().trim
        Result.all(
          List(
            Result.assert(os.read(target / "SKILL.md").contains(feature.value)),
            ref ==== s"refs/heads/${feature.value}",
          )
        )
    }
  }

  private def testDefaultBranch: Result = withRepo(List(feature)) { (temp, repo) =>
    val target = temp / "clone"
    clone(repo, target, none[GitBranch]) match {
      case Left(error) => Result.failure.log(error.toString)
      case Right(_) => Result.assert(os.read(target / "SKILL.md").contains("default content"))
    }
  }

  private def testMissingBranch: Result = withRepo(Nil) { (temp, repo) =>
    clone(repo, temp / "clone", feature.some) match {
      case Left(GitClone.CloneError.MissingBranch(branch, _)) =>
        Result.all(List(branch ==== feature, Result.assert(!os.exists(temp / "clone"))))
      case other => Result.failure.log(other.toString)
    }
  }

  private def testTagOnly: Result = withRepo(Nil) { (temp, repo) =>
    clone(repo, temp / "clone", GitBranch("tag-only").some) match {
      case Left(GitClone.CloneError.MissingBranch(branch, _)) => branch ==== GitBranch("tag-only")
      case other => Result.failure.log(other.toString)
    }
  }

  private def testBranchAndTag: Result = withRepo(List(GitBranch("feature/shared"))) { (temp, repo) =>
    val target = temp / "clone"
    clone(repo, target, GitBranch("feature/shared").some) match {
      case Left(error) => Result.failure.log(error.toString)
      case Right(_) => Result.assert(os.read(target / "SKILL.md").contains("feature/shared"))
    }
  }

  private def testInaccessibleRepo: Result = withRepo(Nil) { (temp, _) =>
    clone(temp / "absent", temp / "clone", feature.some) match {
      case Left(GitClone.CloneError.Failed(failure)) => Result.assert(failure.attempts.nonEmpty)
      case other => Result.failure.log(other.toString)
    }
  }
}
