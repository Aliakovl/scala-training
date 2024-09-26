package dev.aliakovl.gin

import cats.syntax.all._
import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import cats.{Functor, Monad}
import dev.aliakovl.other.MyClass

import java.util.UUID
import scala.util.Random

case class User private (id: UUID, name: String) {
  def this(myName: String) {
    this(UUID.randomUUID(), myName)
  }
}
case class Email(id: UUID, from: UUID, to: UUID, content: String)

object Main {

  implicit val intGen: Gen[Int] = Gen.const(1)

  def main(args: Array[String]): Unit = {
    println(
      Gen.custom[(Int, String)].make.run()
    )

    println(
      Gen
        .custom[Option[Int]]
        .specifyConst(_.when[Some[Int]].value)(-1)
        .make
        .run()
    )

    implicitly[Monad[Gen]].map(Gen.intGen)(_.toString).tap(println)
    implicitly[Functor[Gen]].map(Gen.intGen)(_.toString).tap(println)

    Gen.const(3).map(_.toString).tap(println)

    println(Gen.between(3, 100).flatMap(Gen.string).runWithSeed(11))

    val a =
      List.fill(10000)(Gen.random[Option[Int]]).sequence.map(_.flatten.sum)

    val b = List
      .fill(10000)(
        Gen.custom[Option[Int]].specifyConst(_.when[Some[Int]].value)(1).make
      )
      .sequence
      .map(_.flatten.sum)

    Gen
      .custom[List[Int]]
      .specifyConst(_.when[::[Int]].arg("next"))(List(12341234, 12341234))
      .make
      .tap(println)
      .runWithSeed(41234)

    a.tap(println).runWithSeed(9823745)
    b.tap(println).runWithSeed(9823745)

    9.pure[Gen].flatMap(Gen.alphanumeric).tap(println).runWithSeed(234)
    Gen.alphanumeric(9).tap(println).runWithSeed(234)

    Gen.random[Long].flatMap(_.pure[Gen]).tap(println).runWithSeed(4234)
    Gen.random[Long].tap(println).runWithSeed(4234)

    implicit val ra: Random = new Random(927469873677L)

    Gen
      .between(0, 100)
      .flatMap(Gen.alphanumeric)
      .flatMap(s => Gen.fromFunction { l: (MyClass, MyClass) => (l, s) })
      .tap(println)
      .runWithSeed(927469873677L)

    Gen
      .between(0, 100)
      .flatMap { n =>
        Gen
          .alphanumeric(n)
          .flatMap(s => Gen.fromFunction { l: (MyClass, MyClass) => (l, s) })
      }
      .tap(println)
      .runWithSeed(927469873677L)

    val (
      email,
      fromUser,
      toUser
    ) = (for {
      from <- Gen.random[User]
      to <- Gen.random[User]
      email <- Gen.fromFunction(Email(_, from.id, to.id, _))
    } yield (email, from, to)).runWithSeed(2234)

    assert(email.from == fromUser.id)
    assert(email.to == toUser.id)

    println(email, fromUser, toUser)

    val awe: Gen[User] = Gen.fromFunction { name: String => new User(name) }

    Gen
      .custom[User]
      .specifyConst(_.arg("myName"))("that name")
      .make
      .tap(println)
      .runWithSeed(123)

    Gen.random[UUID] <* Gen(_.setSeed(123)) product Gen
      .random[UUID] tap println runWithSeed 123

    val t: Gen[Seq[UUID => String => Email]] = Gen.function { id: UUID =>
      Gen.function { content: String =>
        Gen.fromFunction(Email(id, _, _, content))
      }
    }.many[Seq](10)

    val tt = Gen.function[(UUID, String), Email] { case (id, content) =>
      Gen.fromFunction(Email(id, _, _, content))
    }
    println(tt.run()((UUID.randomUUID(), "wefwef")))

    println(t.run().map(_.apply(UUID.randomUUID())))
  }
}
