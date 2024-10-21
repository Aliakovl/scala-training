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
}
