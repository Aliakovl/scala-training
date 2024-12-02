package dev.aliakovl.gin

import cats.Eq
import cats.implicits.catsSyntaxEq
import cats.laws.discipline.MonadTests
import org.scalacheck.Arbitrary
import org.scalacheck.{Gen => PGen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class MonadGenLawsSpec
    extends AnyFunSuite
    with FunSuiteDiscipline
    with Checkers {

  checkAll("MonadLaws", MonadTests[Gen].monad[Int, Int, Int])

  private implicit def arbIntGen: Arbitrary[Gen[Int]] = Arbitrary(
    PGen.choose(0, 1000).map(x => Gen.const(x))
  )

  private implicit def arbGenIntInt: Arbitrary[Gen[Int => Int]] = Arbitrary(
    implicitly[Arbitrary[Int => Int]].arbitrary.map(f => Gen.const(f))
  )

  private val seed = 941234514234L

  private implicit def eqGenInt[A: Eq]: Eq[Gen[A]] = (x: Gen[A], y: Gen[A]) =>
    x.runWithSeed(seed).eqv(y.runWithSeed(seed))
}
