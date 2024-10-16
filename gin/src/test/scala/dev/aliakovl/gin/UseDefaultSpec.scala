package dev.aliakovl.gin

import dev.aliakovl.gin.UseDefaultSpec.Insect
import dev.aliakovl.gin.UseDefaultSpec.Insect.{defaultLegs, defaultWings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UseDefaultSpec extends AnyFlatSpec with Matchers {
  behavior of "useDefault"

  it should "generate objects with constructor default parameter" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .make
  ) { all =>
    all.map(_.legs) should contain only defaultLegs
    all.map(_.wings) shouldNot contain only defaultWings()
    all.map(_.toString) shouldNot contain only "insect_6x4"
  }

  it should "generate objects with multiple constructor default parameters" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .useDefault(_.wings)
      .make
  ) { all =>
    all.map(_.legs) should contain only defaultLegs
    all.map(_.wings) should contain only defaultWings()
    all.map(_.toString) shouldNot contain only "insect_6x4"
  }

  it should "generate objects with multiple constructor default parameters in multiple parameter lists" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .useDefault(_.wings)
      .useDefault(_.arg("name"))
      .make
  ) { all =>
    all.map(_.legs) should contain only defaultLegs
    all.map(_.wings) should contain only defaultWings()
    all.map(_.toString) should contain only "insect_6x4"
  }

  it should "generate objects with constructor default parameter using a deep path" in TestCase(
    Gen
      .custom[Option[Insect]]
      .useDefault(_.when[Some[Insect]].value.wings)
      .make
  ) { all =>
    all should contain(None)
    all.flatten shouldNot be(empty)

    all.flatten.map(_.legs) shouldNot contain only defaultLegs
    all.flatten.map(_.wings) should contain only defaultWings()
    all.flatten.map(_.toString) shouldNot contain only "insect_6x4"
  }

}

object UseDefaultSpec {
  case class Insect(legs: Int = defaultLegs, wings: Int = defaultWings())(
      name: String = s"insect_${legs}x$wings"
  ) {
    override def toString: String = name
  }

  object Insect {
    val defaultLegs = 6
    def defaultWings() = 4
  }
}
