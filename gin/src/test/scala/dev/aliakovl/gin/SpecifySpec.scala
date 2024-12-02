package dev.aliakovl.gin

import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpecifySpec extends AnyFlatSpec with Matchers {
  behavior of "specify"

  it should "specify case class parameter" in {
    case class User(age: Int, name: String)

    TestCase(
      Gen
        .custom[User]
        .specify(_.age)(Gen.oneOf(17, 27))
        .make
    ) { elements =>
      every(elements) should matchPattern {
        case User(17, _) =>
        case User(27, _) =>
      }
    }
  }

  it should "specify multiple case class parameter" in {
    case class User(age: Int, name: String)

    TestCase(
      Gen
        .custom[User]
        .specify(_.age)(Gen.oneOf(17, 27))
        .specify(_.name)(Gen.oneOf("Maximus", "Richard"))
        .make
    ).foreach { element =>
      inside(element) { case User(age, name) =>
        Seq(17, 27) should contain(age)
        Seq("Maximus", "Richard") should contain(name)
      }
    }
  }

  it should "specify private class parameter" in {
    class User(age: Int, name: String) {
      def getAge(): Int = age
    }

    TestCase(
      Gen
        .custom[User]
        .specify(_.arg("age"))(Gen.oneOf(17, 27))
        .make
    ).foreach { element =>
      Seq(17, 27) should contain(element.getAge())
    }
  }

  it should "specify multiple parameter in multiple parameter lists" in {
    class User(val age: Int, val name: String)(val id: String)

    TestCase(
      Gen
        .custom[User]
        .specify(_.name)(Gen.oneOf("Maximus", "Richard"))
        .specify(_.age)(Gen.oneOf(17, 27))
        .specify(_.id)(Gen.oneOf("1", "2", "3"))
        .make
    ).foreach { element =>
      Seq("Maximus", "Richard") should contain(element.name)
      Seq(17, 27) should contain(element.age)
      Seq("1", "2", "3") should contain(element.id)
    }
  }

  it should "specify multiple subtypes" in TestCase(
    Gen
      .custom[Either[String, Int]]
      .specify(_.when[Right])(Gen.oneOf(Right(17), Right(27)))
      .specify(_.when[Left])(Gen.oneOf(Left("error"), Left("fail")))
      .make
  ) { elements =>
    every(elements) should matchPattern {
      case Left("error") =>
      case Left("fail")  =>
      case Right(17)     =>
      case Right(27)     =>
    }
  }

  it should "specify deep class parameter" in TestCase(
    Gen
      .custom[Either[String, Option[Int]]]
      .specify(_.when[Right].value.when[Some].value)(Gen.oneOf(17, 27))
      .make
  ) { elements =>
    every(elements) should matchPattern {
      case Left(_)         =>
      case Right(None)     =>
      case Right(Some(17)) =>
      case Right(Some(27)) =>
    }
  }

  it should "specify class parameters on multiple levels" in TestCase(
    Gen
      .custom[Either[String, Option[Int]]]
      .specify(_.when[Right].value)(Gen.oneOf(Some(17), Some(27)))
      .specify(_.when[Left].value)(Gen.oneOf("error", "fail"))
      .make
  ) { elements =>
    every(elements) should matchPattern {
      case Left("error")   =>
      case Left("fail")    =>
      case Right(Some(17)) =>
      case Right(Some(27)) =>
    }
  }

  it should "use the same Gen[T] for recursive type" in TestCase(
    Gen
      .custom[List[Int]]
      .specify(_.when[::].head)(Gen.oneOf(17, 27))
      .make
  ).foreach { element =>
    assert(element.toSet.subsetOf(Set(17, 27)))
  }

  it should "specify deep fields for recursive type" in TestCase(
    Gen
      .custom[List[String]]
      .specify(_.when[::].head)(Gen.oneOf("1", "first"))
      .specify(_.when[::].arg[List[String]]("next").when[::].head)(
        Gen.oneOf("2", "second")
      )
      .specify(
        _.when[::]
          .arg[List[String]]("next")
          .when[::]
          .arg[List[String]]("next")
          .when[::]
          .head
      )(Gen.oneOf("3", "third"))
      .make
  ).foreach { element =>
    def recursive(lst: List[Any]): Boolean = {
      lst should matchPattern {
        case ("1" | "first") :: ("2" | "second") :: ("3" | "third") :: other if recursive(other) =>
        case ("1" | "first") :: ("2" | "second") :: Nil =>
        case ("1" | "first") :: Nil                     =>
        case Nil                                        =>
      }

      true
    }

    recursive(element)
  }

  it should "???" in TestCase {
    def g: Gen[List[String]] = Gen
      .custom[List[String]]
      .specify(_.when[::].arg("next"))(g)
      .make

    g
  }.forall(
    Gen.random[List[String]]
  )(_ shouldBe _)
}
