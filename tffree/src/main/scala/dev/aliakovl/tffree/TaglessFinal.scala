package dev.aliakovl.tffree

object TaglessFinal {
  trait Expr[A] {
    val run: A
  }

  def b(b: Boolean): Expr[Boolean] = new Expr[Boolean] {
    override val run: Boolean = b
  }

  def or(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] =
    new Expr[Boolean] {
      override val run: Boolean = left.run || right.run
    }

  def and(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] =
    new Expr[Boolean] {
      override val run: Boolean = left.run && right.run
    }

  def not(expr: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
    override val run: Boolean = !expr.run
  }

  def i(i: Int): Expr[Int] = new Expr[Int] {
    override val run: Int = i
  }

  def sum(left: Expr[Int], right: Expr[Int]): Expr[Int] = new Expr[Int] {
    override val run: Int = left.run + right.run
  }

  def b2i(b: Expr[Boolean]): Expr[Int] = new Expr[Int] {
    override val run: Int = if (b.run) 1 else 0
  }

  def i2b(i: Expr[Int]): Expr[Boolean] = new Expr[Boolean] {
    override val run: Boolean = i.run != 0
  }
}

object TaglessFinalMain {

  import dev.aliakovl.tffree.TaglessFinal._

  val program: Expr[Int] = sum(b2i(or(b(true), b(false))), i(5))

  def main(args: Array[String]): Unit = {
    val result = program.run
    println(result)
  }
}
