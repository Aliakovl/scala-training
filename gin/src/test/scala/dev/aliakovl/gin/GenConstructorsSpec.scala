package dev.aliakovl.gin

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenConstructorsSpec extends AnyFlatSpec with Matchers {
  behavior of "function"

  it should "create random function" in TestCase(
    Gen.function { a: Int => Gen.string(a) }
  ) { elements =>
    elements.zipWithIndex.foreach { case (f, index) =>
      f(index) should have length index
    }
  }

  behavior of "fromFunction"

  it should "create from generated values" in TestCase(
    Gen.fromFunction { is: (Int, String) => s"${is._1}: ${is._2}" }
  ) { elements =>
    every(elements) should contain(':')
  }

  behavior of "enumerationGen"

  it should "create Gen for Enumeration type" in {
    object Colours extends Enumeration {
      type Colour = Value
      val Blue: Colour = Value("blue-colour")
      val Red: Colour = Value("red-colour")
      val Green: Colour = Value("green-colour")
    }

    TestCase(Gen.random[Colours.Colour]) { elements =>
      every(elements) should matchPattern {
        case Colours.Blue  =>
        case Colours.Red   =>
        case Colours.Green =>
      }
    }
  }

  behavior of "one of"

  it should "create Gen for sum of provided singleton types" in TestCase(
    Gen.one[String].of["A", "B"]
  ) { elements =>
    every(elements) should matchPattern {
      case "A" =>
      case "B" =>
    }
  }

  it should "create Gen for sum of provided sum types" in {
    trait A
    class B extends A
    class C extends A
    class D extends A

    TestCase(Gen.one[A].of[B, C, D]) { elements =>
      every(elements) should matchPattern {
        case _: B =>
        case _: C =>
        case _: D =>
      }
    }
  }
}
