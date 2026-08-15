package aiskills.core.internal

import cats.syntax.all.*

import scala.annotation.tailrec
import scala.quoted.*

/** Compile-time validation behind `GitHubOwnerRepo`.
  *
  * `InlinedRefined` reduces its predicate with `inline if`, which requires a constant condition,
  * and `String.split` cannot be constant-folded. So the literal is unwrapped in a macro and
  * checked there instead, the same way refined4s validates its own `Uri` type.
  */
object GitHubOwnerRepoValidator {

  val UnexpectedLiteralErrorMessage: String =
    """GitHubOwnerRepo must be a String literal.
      |If it is only known at run-time, use `GitHubOwnerRepo.from` or `GitHubOwnerRepo.unsafeFrom` instead.
      |(unsafeFrom is not recommended)""".stripMargin

  /** `owner/repo`: exactly two segments, neither empty. */
  def isValid(ownerRepo: String): Boolean = ownerRepo.split("/").toList match {
    case owner :: repo :: Nil => owner.nonEmpty && repo.nonEmpty
    case _ => false
  }

  def isValidExpr(ownerRepoExpr: Expr[String])(using Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    @tailrec
    def stringLiteralOf(term: Term): Option[String] = term match {
      case Inlined(_, _, inner) => stringLiteralOf(inner)
      case Block(Nil, inner) => stringLiteralOf(inner)
      case Typed(inner, _) => stringLiteralOf(inner)
      case Literal(StringConstant(value)) => value.some
      case _ => none[String]
    }

    stringLiteralOf(ownerRepoExpr.asTerm) match {
      case Some(ownerRepo) => Expr(isValid(ownerRepo))
      case None =>
        report.error(UnexpectedLiteralErrorMessage, ownerRepoExpr)
        Expr(false)
    }
  }
}
