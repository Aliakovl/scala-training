package dev.aliakovl.gin

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpecifyConstSpec extends AnyFlatSpec with Matchers {
  behavior of "specifyConst"

  it should "specify case class parameter" in {
    case class User(age: Int, name: String)

    TestCase(
      Gen
        .custom[User]
        .specifyConst(_.age)(19)
        .make
    ) { elements =>
      every(elements) should have(
        'age(19)
      )
    }
  }

  it should "specify multiple case class parameter" in {
    case class User(age: Int, name: String)

    TestCase(
      Gen
        .custom[User]
        .specifyConst(_.age)(19)
        .specifyConst(_.name)("Ann")
        .make
    ) { elements =>
      every(elements) should have(
        'age(19),
        'name("Ann")
      )
    }
  }

  it should "specify private class parameter" in {
    class User(age: Int, name: String) {
      def getAge(): Int = age
    }

    TestCase(
      Gen
        .custom[User]
        .specifyConst(_.arg("age"))(19)
        .make
    ) { elements =>
      every(elements) should have(
        'age(19)
      )
    }

  }

  it should "specify multiple parameter in multiple parameter lists" in {
    class User(val age: Int, val name: String)(val id: String)

    TestCase(
      Gen
        .custom[User]
        .specifyConst(_.name)("Igor")
        .specifyConst(_.age)(23)
        .specifyConst(_.id)("uno")
        .make
    ) { elements =>
      every(elements) should have(
        'age(23),
        'name("Igor"),
        'id("uno")
      )
    }
  }

  it should "specify multiple subtypes" in TestCase(
    Gen
      .custom[Either[String, Int]]
      .specifyConst(_.when[Right])(Right(54))
      .specifyConst(_.when[Left])(Left("error"))
      .make
  ) { elements =>
    every(elements) should matchPattern {
      case Left("error") =>
      case Right(54)     =>
    }
  }

  it should "specify deep class parameter" in TestCase(
    Gen
      .custom[Either[String, Option[Int]]]
      .specifyConst(_.when[Right].value.when[Some].value)(20)
      .make
  ) { elements =>
    every(elements) should matchPattern {
      case Left(_)         =>
      case Right(None)     =>
      case Right(Some(20)) =>
    }
  }

  it should "specify class parameters on multiple levels" in TestCase(
    Gen
      .custom[Either[String, Option[Int]]]
      .specifyConst(_.when[Right].value.when[Some])(Some(20))
      .specifyConst(_.when[Left].value)("unique")
      .make
  ) { elements =>
    every(elements) should matchPattern {
      case Left("unique")  =>
      case Right(None)     =>
      case Right(Some(20)) =>
    }
  }

  it should "use the same Gen[T] for recursive type (1)" in TestCase(
    Gen
      .custom[List[String]]
      .specifyConst(_.when[::].head)("15")
      .make
  ) { elements =>
    every(elements) should (be(empty) or contain only "15")
  }

  it should "use the same Gen[T] for recursive type (2)" in TestCase(
    Gen
      .custom[List[String]]
      .specifyConst(_.when[::[String]].head)("15")
      .make
  ) { elements =>
    every(elements) should (be(empty) or contain only "15")
  }

  it should "specify deep fields for recursive type" in TestCase(
    Gen
      .custom[List[String]]
      .specifyConst(_.when[::].head)("first")
      .specifyConst(_.when[::].arg[List[String]]("next").when[::].head)("second")
      .specifyConst(_.when[::].arg[List[String]]("next").when[::].arg[List[String]]("next").when[::].head)("third")
      .make
  ).foreach { element =>
    def recursive(lst: List[Any]): Boolean = {
      lst should matchPattern {
        case "first" :: "second" :: "third" :: other if recursive(other) =>
        case "first" :: "second" :: Nil                                  =>
        case "first" :: Nil                                              =>
        case Nil                                                         =>
      }

      true
    }

    recursive(element)
  }

  it should "specify implicit parameter" in {
    case class T(int: Int)(implicit str: String) {
      def getStr: String = str
    }

    implicit val implStr: String = "implicit"

    TestCase(
      Gen
        .custom[T]
        .specifyConst(_.arg("str"))("const")
        .make
    ) { elements =>
      every(elements) should matchPattern {
        case t: T if t.getStr == "const" =>
      }
    }
  }

  it should "???" in TestCase(
    Gen
      .custom[Either[String, Option[Int]]]
      .specifyConst(_.when[Right].value.when[Some])(Some(10))
      .make
  ).forall(
    Gen
      .custom[Either[String, Option[Int]]]
      .specifyConst(_.when[Right].value.when[Some].value)(10)
      .make
  )(_ shouldBe _)
}
