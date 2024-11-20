package dev.aliakovl.gin

import dev.aliakovl.gin.GenCustomSpec._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenCustomSpec extends AnyFlatSpec with Matchers {
  behavior of "custom"

  it should "generate all concrete children" in TestCase(
    Gen.custom[A].make
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

    Gen.custom[Option[A]].make
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

    Gen.custom[Person].make
  } { elements =>
    all(elements) should have(
      'name("sample-name"),
      'age(21)
    )
  }

  it should "generate class with multiple parameter lists" in TestCase {
    implicit val aGen: Gen[A] = Gen.const(D)
    implicit val strGen: Gen[String] = Gen.const("second-value")
    implicit val personGen: Gen[Person] = Gen.const(new Person("weirdo", -2))

    Gen.custom[WierdClass].make
  } { elements =>
    all(elements) should have(
      'a(D),
      'str("second-value"),
      'name("weirdo"),
      'age(-2)
    )
  }

  it should "generate class with implicit parameter got from implicit scope" in TestCase {
    implicit val value: Gen[String] = Gen.const("value")
    implicit val impl: String = "implicit"

    Gen.custom[ClassWithImplicit].make
  } { elements =>
    all(elements) should have(
      'value("value"),
      'impl("implicit")
    )
  }

  it should "be possible to assign it to implicit value" in TestCase {
    implicit val personGen: Gen[Person] = {
      Gen.custom[Person].specifyConst(_.arg("age"))(123).make
    }

    Gen.random[Person]
  } { elements =>
    all(elements) should have(
      'age(123)
    )
  }

  it should "use the same Gen[T] for inner parameters" in TestCase {
    implicit val genList: Gen[List[Int]] = Gen.const(List(2))

    Gen.custom[List[Int]].specifyConst(_.when[::[Int]].head)(1).make
  } { elements =>
    all(elements) should (be (empty) or contain only 1)
  }

  it should "use then same Gen[T] for recursive type implicitly" in TestCase {
    implicit lazy val genList: Gen[List[Int]] =
      Gen.custom[List[Int]].specifyConst(_.when[::[Int]].head)(1).make

    Gen.random[List[Int]]
  } { elements =>
    all(elements) should (be (empty) or contain only 1)
  }

  it should "use then same Gen[T] for recursive type manually" in TestCase {
    lazy val genList: Gen[List[Int]] =
      Gen
        .custom[List[Int]]
        .specifyConst(_.when[::[Int]].head)(1)
        .specify(_.when[::[Int]].arg("next"))(genList)
        .make

    genList
  } { elements =>
    all (elements) should (be (empty) or contain only 1)
  }

  it should "use implicit method to make Gen[T] with inner macro implicit" in TestCase {
    implicit val numberGen: Gen[Int] = Gen.const(42)
    implicit def optionGen[T: Gen]: Gen[Option[T]] = Gen.random[T].map(Some(_))

    Gen.custom[ComplexClass].make
  } { elements =>
    all(elements) should matchPattern {
      case ComplexClass(42, Some(_)) =>
    }
  }

  it should "use implicit method to make Gen[T]" in TestCase {
    implicit def intGen: Gen[Int] = Gen.const(42)
    implicit def optionGen[T: Gen]: Gen[Option[T]] = Gen.random[T].map(Some(_))

    Gen.random[Option[Int]]
  } { elements =>
    all(elements) should matchPattern {
      case Some(42) =>
    }
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

  class WierdClass(a: A, str: String)(person: Person) {
    def getA: A = a
    def getStr: String = str
    def getName: String = person.getName
    def getAge: Int = person.getAge
  }

  class ClassWithImplicit(value: String)(implicit impl: String) {
    def getValue: String = value
    def getImpl: String = impl
  }

  case class ComplexClass(number: Int, a: Option[A])
}
