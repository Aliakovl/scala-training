package dev.aliakovl.gin

import dev.aliakovl.gin.ExcludeSpec._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExcludeSpec extends AnyFlatSpec with Matchers {
  behavior of "exclude"

  it should "exclude final class" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[B])
      .make
  ) { elements =>
    no(elements) shouldBe a[B]
  }

  it should "exclude case class" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[C])
      .make
  ) { elements =>
    no(elements) shouldBe a[C]
  }

  it should "exclude case object" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[D.type])
      .make
  ) { elements =>
    no(elements) shouldBe a[D.type]
  }

  it should "exclude object" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[E.type])
      .make
  ) { elements =>
    no(elements) shouldBe an[E.type]
  }

  it should "exclude sealed trait" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[F])
      .make
  ) { elements =>
    no(elements) shouldBe an[F]
  }

  it should "exclude deeper class" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[F1])
      .make
  ) { elements =>
    no(elements) shouldBe an[F1]
  }

  it should "exclude sealed abstract class" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[G])
      .make
  ) { elements =>
    no(elements) shouldBe a[G]
  }
}

object ExcludeSpec {
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
