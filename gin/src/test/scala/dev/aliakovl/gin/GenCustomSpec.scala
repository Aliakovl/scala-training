package dev.aliakovl.gin

import dev.aliakovl.gin.GenCustomSpec._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenCustomSpec extends AnyFlatSpec with Matchers {
  behavior of "custom"

  it should "generate all concrete children" in TestCase(
    Gen
      .custom[A]
      .make
  ) { elements =>
    atLeast(1, elements) shouldBe a[B]
    atLeast(1, elements) shouldBe a[C]
    atLeast(1, elements) shouldBe a[D.type]
    atLeast(1, elements) shouldBe an[E.type]
    atLeast(1, elements) shouldBe an[F1]
    atLeast(1, elements) shouldBe a[G1]
  }

  it should "generate values with generator from implicit scope" in TestCase {
    implicit val aGen: Gen[A] = Gen.one[A].of[C, F, D.type]

    Gen
      .custom[Option[A]]
      .make
  } { elements =>
    val definedElements = elements.flatten

    atLeast(1, elements) should not be defined
    no(definedElements) shouldBe a[B]
    atLeast(1, definedElements) shouldBe a[C]
    atLeast(1, definedElements) shouldBe a[D.type]
    no(definedElements) shouldBe an[E.type]
    atLeast(1, definedElements) shouldBe an[F1]
    no(definedElements) shouldBe a[G1]
  }

  it should "generate simple class with parameters" in TestCase {
    implicit val nameGen: Gen[String] = Gen.const("sample-name")
    implicit val ageGen: Gen[Int] = Gen.const(21)

    Gen
      .custom[Person]
      .make
  } { elements =>
    all(elements) should have (
      'name ("sample-name"),
      'age (21)
    )
  }
}

object GenCustomSpec {
  sealed trait A
  final class B extends A
  case class C() extends A
  case object D extends A
  object E extends A
  sealed trait F extends A
  final class F1 extends F
  sealed abstract class G extends A
  final class G1 extends G

  class Person(name: String, age: Int) {
    def getName: String = name
    def getAge: Int = age
  }
}
