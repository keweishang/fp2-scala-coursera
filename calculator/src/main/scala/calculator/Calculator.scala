package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues(signal => Signal(eval(signal(), namedExpressions, Set())))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], visited: Set[String]): Double = expr match {
    case Literal(v) => v
    case Ref(n) => {
      if (visited contains n) Double.NaN
      else eval(references.get(n).get(), references, visited + n)
    }
    case Plus(a, b) => eval(a, references, visited) + eval(b, references, visited)
    case Minus(a, b) => eval(a, references, visited) - eval(b, references, visited)
    case Times(a, b) => eval(a, references, visited) * eval(b, references, visited)
    case Divide(a, b) => eval(a, references, visited) / eval(b, references, visited)

  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
