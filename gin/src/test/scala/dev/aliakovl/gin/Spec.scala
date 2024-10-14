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

      "child trait is not sealed" in {
        sealed trait A
        trait B extends A
        final class C extends B
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "child abstract class is not sealed" in {
        sealed trait A
        abstract class B extends A
        final class C extends B
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "child class is neither final nor a case class" in {
        sealed trait A
        class B extends A
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "class does not have any public constructors" in {
        final class A private {}
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
      }

      "type parameters in .when[T[...]] are narrowed" in {
        sealed trait A
        final class B extends A
        """
          |Gen
          |  .custom[Option[A]]
          |  .specifyConst(_.when[Some[B]])(Some(new B))
          |  .make
          |""".stripMargin shouldNot compile
      }

      "could not find value for implicit parameter" in {
        class A(implicit param: Int)
        class B(val value: Int)(implicit param: Int)
        "Gen.custom[A].make" shouldNot compile
        "Gen.custom[Option[A]].make" shouldNot compile
        "Gen.custom[B].make" shouldNot compile
        "Gen.custom[Option[B]].make" shouldNot compile
      }

      "param does not have default argument" in {
        case class A(param: Int)
        "Gen.custom[A].useDefault(_.param).make" shouldNot compile
        "Gen.custom[Option[A]].useDefault(_.when[Some[A]].value.param).make" shouldNot compile
      }

      "path in .useDefault(...) ends with .when[...]" in {
        case class A(param: Int = 1234)
        "Gen.custom[A].useDefault(_.when[A]).make" shouldNot compile
        "Gen.custom[Option[A]].useDefault(_.when[Some[A]]).make" shouldNot compile
        "Gen.custom[Option[A]].useDefault(_.when[Some]).make" shouldNot compile
      }

      "all concrete children is excluded" in {
        sealed trait A
        final class B extends A
        case class C() extends A
        case object D extends A
        object E extends A

        """
          |Gen
          |  .custom[A]
          |  .exclude(_.when[B])
          |  .exclude(_.when[C])
          |  .exclude(_.when[D.type])
          |  .exclude(_.when[E.type])
          |  .make
          |""".stripMargin shouldNot compile

        "Gen.custom[A].exclude(_.when[A]).make" shouldNot compile

        "Gen.custom[Option[A]].exclude(_.when[Option]).make" shouldNot compile

        """
          |Gen
          |  .custom[Option[A]]
          |  .exclude(_.when[Some].value.when[B])
          |  .exclude(_.when[Some].value.when[C])
          |  .exclude(_.when[Some].value.when[D.type])
          |  .exclude(_.when[Some].value.when[E.type])
          |  .make
          |""".stripMargin shouldNot compile
      }

      "path in .exclude(...) ends with param" in {
        sealed trait A
        final class B extends A
        final class C extends A

        """
          |Gen
          |  .custom[Option[A]]
          |  .exclude(_.when[Some[A]].value)
          |  .make
          |""".stripMargin shouldNot compile
      }

      "path contains wrong parameter" in {
        """
          |Gen
          |  .custom[Either[Int, Boolean]]
          |  .specifyConst(_.when[Right].isRight)(true)
          |  .make
          |""".stripMargin shouldNot compile
      }
    }

    "compile" when {
      "there is no need to generate wrong type A" in {
        trait A

        """
          |Gen
          |  .custom[Either[String, A]]
          |  .specifyConst(_.when[Right[String, A]].value)(new A {})
          |  .make
          |""".stripMargin should compile

        """
          |Gen
          |  .custom[Either[String, A]]
          |  .exclude(_.when[Right[String, A]])
          |  .make
          |""".stripMargin should compile

      }
    }
  }
}
