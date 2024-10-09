package dev.aliakovl.gin

import cats.data.Ior
import cats.syntax.all._
import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import cats.{Functor, Monad}
import dev.aliakovl.other._

import java.util.UUID
import scala.util.Random

case class User private (id: UUID, name: String) {
  def this(myName: String) {
    this(UUID.randomUUID(), myName)
  }
}
case class Email(id: UUID, from: UUID, to: UUID, content: String)

case class Jwelfkn(k: Int) extends AnyVal

object Main {

  implicit val intGen: Gen[Int] = Gen.const(1)

  def main(args: Array[String]): Unit = {
    Gen
      .custom[(Int, String)]
      .make
      .tap(println)
      .run()

    Gen
      .custom[Option[Int]]
      .specifyConst(_.when[Some].value)(-1)
      .make
      .tap(println)
      .run()

    Gen
      .custom[Either[String, Int]]
      .specifyConst(_.when[Right].value)(3)
      .make
      .tap(println)
      .run()

    implicitly[Monad[Gen]].map(Gen.intGen)(_.toString).tap(println)
    implicitly[Functor[Gen]].map(Gen.intGen)(_.toString).tap(println)

    Gen.const(3).map(_.toString).tap(println)

    println(Gen.between(3, 100).flatMap(Gen.string).runWithSeed(11))

    val a =
      List.fill(10000)(Gen.random[Option[Int]]).sequence.map(_.flatten.sum)

    val b = List
      .fill(10000)(
        Gen.custom[Option[Int]].specifyConst(_.when[Some].value)(1).make
      )
      .sequence
      .map(_.flatten.sum)

    Gen
      .custom[List[Int]]
      .specifyConst(_.when[::].arg("next"))(List(12341234, 12341234))
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

    val t: Gen[Seq[UUID => String => Email]] = Gen
      .function { id: UUID =>
        Gen.function { content: String =>
          Gen.fromFunction(Email(id, _, _, content))
        }
      }
      .many[Seq](10)

    println(t.run().map(_.apply(UUID.randomUUID())))

    val tt = Gen.function[(UUID, String), Email] { case (id, content) =>
      Gen.fromFunction(Email(id, _, _, content))
    }

    tt.ap(Gen.product(Gen.random[UUID], Gen.alphanumeric(100)))
      .tap(println)
      .run()

    Gen.random[Short].tap(println).run()

    Gen.make(Jwelfkn(234)).tap(println).run()

    Gen
      .custom[MyClass]
      .specifyConst(_.when[MyClass1])(MyClass1())
      .specifyConst(_.when[J])(new J(";lkjewflkqjhefw"))
      .exclude(_.when[MyClass3])
      .make
      .many[List](10)
      .map(_.mkString("\t"))
      .tap(println)
      .run()

    Gen
      .custom[Option[MyClass]]
      .specifyConst(_.when[None.type])(None)
      .make
      .many[List](10)
      .tap(println)
      .run()

    Gen
      .custom[Option[String]]
      .specifyConst(_.when[Option])(None)
      .make
      .tap(println)
      .run()

    Gen
      .custom[Either[String, MyClass]]
      .exclude(_.when[Right].value.when[MyClass1])
      .exclude(_.when[({ type LL[A] = Left[A, MyClass] })#LL])
      .useDefault(_.when[Right].value.when[MyClass2].mc2field)
      .specifyConst(_.when[Right].value.when[J])(new J("ahefjkl"))
      .make
      .tap(println)
      .run()

    sealed abstract class Clazz
    case class A() extends Clazz
    case class B() extends Clazz
    case class C() extends Clazz

    sealed trait Opt[A]
    case class Noth[A]() extends Opt[A]
    case class Maybe[A](value: A) extends Opt[A]

    Gen
      .custom[Option[Clazz]]
      .exclude(_.when[None.type])
      .make
      .many[List](10000)
      .map(_.count {
        case Some(A()) => true
        case Some(B()) => true
        case Some(C()) => true
        case None      => false
      })
      .tap(println)
      .run()

    Gen
      .custom[Clazz]
      .exclude(_.when[A])
      .make
      .tap(println)
      .run()

    Gen
      .custom[Opt[Clazz]]
      .exclude(_.when[Maybe[Clazz]].value.when[A])
      .exclude(_.when[Noth[Clazz]])
      .make
      .many[List](10000)
      .map(_.count {
        case Maybe(A()) => false
        case Maybe(B()) => true
        case Maybe(C()) => true
        case Noth()     => false
      })
      .tap(println)
      .run()

    Gen
      .custom[Ior[String, Int]]
      .specifyConst(_.when[Ior.Right].b)(2)
      .specifyConst(_.when[Ior.Left].a)("two")
      .exclude(_.when[Ior.Both])
      .make
      .many[List](10)
      .tap(println)
      .run()

  }
}
