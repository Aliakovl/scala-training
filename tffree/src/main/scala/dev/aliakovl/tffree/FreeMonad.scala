package dev.aliakovl.tffree

import dev.aliakovl.core.Functor._
import dev.aliakovl.core.Monad._
import dev.aliakovl.core.data.IO
import dev.aliakovl.core.{Monad, ~>}
import dev.aliakovl.tffree.FreeMonad.ExprAlg
import dev.aliakovl.free.Free

object FreeMonad {
  sealed trait ExprAlg[A]
  final case class B(b: Boolean) extends ExprAlg[Boolean]
  final case class Or(left: Boolean, right: Boolean) extends ExprAlg[Boolean]
  final case class And(left: Boolean, right: Boolean) extends ExprAlg[Boolean]
  final case class Not(expr: Boolean) extends ExprAlg[Boolean]
  final case class I(i: Int) extends ExprAlg[Int]
  final case class Sum(left: Int, right: Int) extends ExprAlg[Int]
  final case class B2I(b: Boolean) extends ExprAlg[Int]
  final case class I2B(i: Int) extends ExprAlg[Boolean]

  type Expr[A] = Free[ExprAlg, A]

  object ExprAlg {
    def b(b: Boolean): Expr[Boolean] = Free.liftM(B(b))

    def or(left: Boolean, right: Boolean): Expr[Boolean] =
      Free.liftM(Or(left, right))

    def and(left: Boolean, right: Boolean): Expr[Boolean] =
      Free.liftM(And(left, right))

    def not(expr: Boolean): Expr[Boolean] = Free.liftM(Not(expr))

    def i(i: Int): Expr[Int] = Free.liftM(I(i))

    def sum(left: Int, right: Int): Expr[Int] = Free.liftM(Sum(left, right))

    def b2i(b: Boolean): Expr[Int] = Free.liftM(B2I(b))

    def i2b(i: Int): Expr[Boolean] = Free.liftM(I2B(i))
  }

  implicit val eval: ExprAlg ~> EvalExpr = new (ExprAlg ~> EvalExpr) {
    override def apply[A](fa: ExprAlg[A]): EvalExpr[A] = fa match {
      case B(b)             => EvalExpr(b)
      case Or(left, right)  => EvalExpr(left || right)
      case And(left, right) => EvalExpr(left && right)
      case Not(expr)        => EvalExpr(!expr)
      case I(i)             => EvalExpr(i)
      case Sum(left, right) => EvalExpr(left + right)
      case B2I(b)           => EvalExpr(if (b) 1 else 0)
      case I2B(i)           => EvalExpr(i != 0)
    }
  }

  def printIO(a: => String): IO[Unit] = {
    IO.pure(()).map(_ => println(a))
  }

  implicit val tostring: ExprAlg ~> IO = new (ExprAlg ~> IO) {
    override def apply[A](fa: ExprAlg[A]): IO[A] = fa match {
      case B(b) => IO.pure(b)
      case Or(left, right) => {
        val res = left || right
        val str = s"$left | $right = $res"
        printIO(str).as(res)
      }
      case And(left, right) => {
        val res = left && right
        val str = s"$left & $right = $res"
        printIO(str).as(res)
      }
      case Not(expr) => {
        val res = !expr
        val str = s"~$expr = $res"
        printIO(str).as(res)
      }
      case I(i) => IO.pure(i)
      case Sum(left, right) => {
        val res = left + right
        val str = s"$left + $right = $res"
        printIO(str).as(res)
      }
      case B2I(b) => IO.pure(if (b) 1 else 0)
      case I2B(i) => IO.pure(i != 0)
    }
  }
}

object FreeMonadMain {

  def program[F[_]: Monad](implicit eval: ExprAlg ~> F): F[Int] = {
    import dev.aliakovl.tffree.FreeMonad.ExprAlg._

    (for {
      o <- or(left = true, right = false).flatMap(b2i)
      s <- sum(o, 5)
    } yield s).foldMap[F]
  }

  def main(args: Array[String]): Unit = {
    val result = program[EvalExpr].run
    println(result)
    program[IO].run()
  }
}
