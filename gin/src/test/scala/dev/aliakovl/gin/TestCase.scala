package dev.aliakovl.gin

import org.scalatest.Assertion

case class TestCase[A](gen: Gen[A]) {
  private val testCount = 100
  private val seed = 599311303609L

  private def generate(genA: Gen[A]): Seq[A] =
    genA.many[Seq](testCount).runWithSeed(seed)

  def apply(checks: Seq[A] => Any): Unit = {
    checks(generate(gen))
  }

  def foreach(check: A => Any): Unit = {
    generate(gen).foreach(check)
  }

  def forall(other: Gen[A])(f: (A, A) => Assertion): Unit = {
    generate(gen)
      .zip(generate(other))
      .foreach(f.tupled)
  }
}
