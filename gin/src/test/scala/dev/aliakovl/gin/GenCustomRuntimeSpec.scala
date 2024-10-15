package dev.aliakovl.gin

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GenCustomRuntimeSpec extends AnyWordSpec with Matchers {
  case class Insect(legs: Int = 6, wings: Int = 4)(val name: String = "insect")

  "Gen.custom" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .make
  ) { list =>
    list should have size 100
    list.map(_.legs) should contain only 6
    list.map(_.legs) should contain only 6
    list.map(_.wings) shouldNot contain only 4
    list.map(_.name) shouldNot contain only "insect"
  }

  "f" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .useDefault(_.wings)
      .make
  ) { list =>
    list should have size 100
    list.map(_.legs) should contain only 6
    list.map(_.legs) should contain only 6
    list.map(_.wings) should contain only 4
    list.map(_.name) shouldNot contain only "insect"
  }

  "s" in TestCase(
    Gen
      .custom[Insect]
      .useDefault(_.legs)
      .useDefault(_.wings)
      .useDefault(_.name)
      .make
  ) { list =>
    list.map(_.legs) should contain only 6
    list.map(_.legs) should contain only 6
    list.map(_.wings) should contain only 4
    list.map(_.name) should contain only "insect"
  }
}
