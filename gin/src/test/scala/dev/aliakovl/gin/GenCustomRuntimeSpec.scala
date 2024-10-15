package dev.aliakovl.gin

import dev.aliakovl.gin.GenCustomRuntimeSpec.Insect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenCustomRuntimeSpec extends AnyFlatSpec with Matchers {
  behavior of "useDefault"

  it should "generate objects with constructor default parameter" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .make
  ) { list =>
    list.map(_.legs) should contain only 6
    list.map(_.wings) shouldNot contain only 4
    list.map(_.toString) shouldNot contain only "insect_6x4"
  }

  it should "generate objects with multiple constructor default parameters" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .useDefault(_.wings)
      .make
  ) { list =>
    list.map(_.legs) should contain only 6
    list.map(_.wings) should contain only 4
    list.map(_.toString) shouldNot contain only "insect_6x4"
  }

  it should "generate objects with multiple constructor default parameters in multiple parameter lists" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .useDefault(_.wings)
      .useDefault(_.arg("name"))
      .make
  ) { list =>
    list.map(_.legs) should contain only 6
    list.map(_.wings) should contain only 4
    list.map(_.toString) should contain only "insect_6x4"
  }
}

object GenCustomRuntimeSpec {
  case class Insect(legs: Int = 6, wings: Int = 4)(
      name: String = s"insect_${legs}x$wings"
  ) {
    override def toString: String = name
  }
}
