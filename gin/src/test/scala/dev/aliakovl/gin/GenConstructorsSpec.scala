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
      elements should contain allElementsOf Colours.values
    }
  }
}
