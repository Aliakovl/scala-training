package dev.aliakovl.gin

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Spec extends AnyFlatSpec with Matchers {
  "tuple" should "compile" in {
    "Gen.custom[(Int, String)].make" should compile
  }

  "int" should "not compile" in {
    "Gen.custom[Int].make" shouldNot compile
  }
}
