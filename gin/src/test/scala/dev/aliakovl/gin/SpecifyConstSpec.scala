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
      all(elements) should have(
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
      all(elements) should have(
        'age(19),
        'name("Ann")
      )
    }
  }

  it should "specify class parameter" in {
    class User(age: Int, name: String) {
      def getAge(): Int = age
    }

    TestCase(
      Gen
        .custom[User]
        .specifyConst(_.arg("age"))(19)
        .make
    ) { elements =>
      all(elements) should have(
        'age(19)
      )
    }

  }

  it should "specify multiple parameter in multiple parameter lists" in {
    class User(val age: Int, val name: String)(val id: String) {
      def getAge(): Int = age
      def getName(): String = name
      def getId(): String = id
    }

    TestCase(
      Gen
        .custom[User]
        .specifyConst(_.name)("Igor")
        .specifyConst(_.age)(23)
        .specifyConst(_.id)("uno")
        .make
    ) { elements =>
      all(elements) should have(
        'age(23),
        'name("Igor"),
        'id("uno")
      )
    }
  }

  it should "specify inner class parameter" in TestCase(
    Gen
      .custom[Either[String, Option[Int]]]
      .specifyConst(_.when[Right].value.when[Some].value)(11)
      .make
  ) { elements =>
    all(elements) should matchPattern {
      case Left(_)         =>
      case Right(None)     =>
      case Right(Some(11)) =>
    }
  }
}
