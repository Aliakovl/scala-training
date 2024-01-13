package dev.aliakovl.tffree

object TaglessInitial {
  sealed trait Expr[A]
  final case class B(b: Boolean) extends Expr[Boolean]
  final case class Or(left: Expr[Boolean], right: Expr[Boolean])
      extends Expr[Boolean]
  final case class And(left: Expr[Boolean], right: Expr[Boolean])
      extends Expr[Boolean]
  final case class Not(expr: Expr[Boolean]) extends Expr[Boolean]
  final case class I(i: Int) extends Expr[Int]
  final case class Sum(left: Expr[Int], right: Expr[Int]) extends Expr[Int]
  final case class B2I(b: Expr[Boolean]) extends Expr[Int]
  final case class I2B(i: Expr[Int]) extends Expr[Boolean]

  def eval[A](alg: Expr[A]): A = alg match {
    case B(b)             => b
    case Or(left, right)  => eval(left) || eval(right)
    case And(left, right) => eval(left) && eval(right)
    case Not(expr)        => !eval(expr)
    case I(i)             => i
    case Sum(left, right) => eval(left) + eval(right)
    case B2I(b)           => if (eval(b)) 1 else 0
    case I2B(i)           => eval(i) != 0
  }

  def asString[A](alg: Expr[A]): String = alg match {
    case B(b)             => if (b) "1" else "0"
    case Or(left, right)  => s"(${asString(left)}) | (${asString(right)})"
    case And(left, right) => s"(${asString(left)}) & (${asString(right)})"
    case Not(expr)        => s"~(${asString(expr)})"
    case I(i)             => s"$i"
    case Sum(left, right) => s"(${asString(left)}) + (${asString(right)})"
    case B2I(b)           => s"${asString(b)}"
    case I2B(i)           => s"${asString(i)}"
  }
}

object TaglessInitialMain {

  import dev.aliakovl.tffree.TaglessInitial._

  val program: Expr[Int] = Sum(B2I(Or(B(true), B(false))), I(5))

  def main(args: Array[String]): Unit = {
    val result = eval(program)
    val string = asString(program)
    println(result)
    println(string)
  }
}
