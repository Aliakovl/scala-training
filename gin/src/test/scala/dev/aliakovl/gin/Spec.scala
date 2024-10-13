package dev.aliakovl.gin

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Spec extends AnyWordSpec with Matchers {
  "Gen.custom" should {
    "not compile" when {
      "trait is not sealed" in {
        trait A
        final class B extends A
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "abstract class is not sealed" in {
        abstract class A
        final class B extends A
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "sealed trait does not have child classes" in {
        sealed trait A
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "sealed abstract class does not have child classes" in {
        sealed abstract class A
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "child class is neither final nor a case class" in {
        sealed trait A
        class B extends A
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

    }
  }
}
