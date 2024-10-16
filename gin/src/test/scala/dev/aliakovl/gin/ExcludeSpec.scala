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
  ) { all =>
    all should not contain a[B]
  }

  it should "exclude case class" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[C])
      .make
  ) { all =>
    all should not contain a[C]
  }

  it should "exclude case object" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[D.type])
      .make
  ) { all =>
    all should not contain a[D.type]
  }

  it should "exclude object" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[E.type])
      .make
  ) { all =>
    all should not contain an[E.type]
  }

  it should "exclude sealed trait" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[F])
      .make
  ) { all =>
    all should not contain an[F]
  }

  it should "exclude deeper class" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[F1])
      .make
  ) { all =>
    all should not contain an[F1]
  }

  it should "exclude sealed abstract class" in TestCase(
    Gen
      .custom[A]
      .exclude(_.when[G])
      .make
  ) { all =>
    all should not contain a[G]
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
