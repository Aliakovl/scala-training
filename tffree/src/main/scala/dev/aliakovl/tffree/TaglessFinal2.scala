package dev.aliakovl.tffree

object TaglessFinal2 {
  trait Algebra[F[_]] {
    def b(b: Boolean): F[Boolean]
    def or(left: F[Boolean], right: F[Boolean]): F[Boolean]
    def and(left: F[Boolean], right: F[Boolean]): F[Boolean]
    def not(expr: F[Boolean]): F[Boolean]
    def i(i: Int): F[Int]
    def sum(left: F[Int], right: F[Int]): F[Int]
    def b2i(b: F[Boolean]): F[Int]
    def i2b(i: F[Int]): F[Boolean]
  }

  implicit val expr: Algebra[EvalExpr] = new Algebra[EvalExpr] {
    override def b(b: Boolean): EvalExpr[Boolean] = EvalExpr(b)

    override def or(
        left: EvalExpr[Boolean],
        right: EvalExpr[Boolean]
    ): EvalExpr[Boolean] = EvalExpr(left.run || right.run)

    override def and(
        left: EvalExpr[Boolean],
        right: EvalExpr[Boolean]
    ): EvalExpr[Boolean] = EvalExpr(left.run && right.run)

    override def not(expr: EvalExpr[Boolean]): EvalExpr[Boolean] = EvalExpr(
      !expr.run
    )

    override def i(i: Int): EvalExpr[Int] = EvalExpr(i)

    override def sum(left: EvalExpr[Int], right: EvalExpr[Int]): EvalExpr[Int] =
      EvalExpr(left.run + right.run)

    override def b2i(b: EvalExpr[Boolean]): EvalExpr[Int] = EvalExpr(
      if (b.run) 1 else 0
    )

    override def i2b(i: EvalExpr[Int]): EvalExpr[Boolean] = EvalExpr(
      i.run != 0
    )
  }

  implicit val tostring: Algebra[ToString] = new Algebra[ToString] {
    override def b(b: Boolean): ToString[Boolean] = ToString(
      if (b) "1" else "0"
    )

    override def or(
        left: ToString[Boolean],
        right: ToString[Boolean]
    ): ToString[Boolean] = ToString(s"(${left.run}) | (${right.run})")

    override def and(
        left: ToString[Boolean],
        right: ToString[Boolean]
    ): ToString[Boolean] = ToString(s"(${left.run}) & (${right.run})")

    override def not(expr: ToString[Boolean]): ToString[Boolean] = ToString(
      s"~(${expr.run})"
    )

    override def i(i: Int): ToString[Int] = ToString(s"$i")

    override def sum(left: ToString[Int], right: ToString[Int]): ToString[Int] =
      ToString(s"(${left.run}) + (${right.run})")

    override def b2i(b: ToString[Boolean]): ToString[Int] = ToString(b.run)

    override def i2b(i: ToString[Int]): ToString[Boolean] = ToString(i.run)
  }
}

object TaglessFinal2Main {

  import dev.aliakovl.tffree.TaglessFinal2.Algebra

  def program[F[_]](implicit alg: Algebra[F]): F[Int] = {
    import alg._
    sum(b2i(or(b(true), b(false))), i(5))
  }

  def main(args: Array[String]): Unit = {
    val result = program[EvalExpr].run
    val string = program[ToString].run
    println(result)
    println(string)
  }
}
