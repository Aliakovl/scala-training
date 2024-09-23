package dev.aliakovl.gin

import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import cats.{Functor, Monad}
import dev.aliakovl.other.MyClass

object Main {

  implicit val intGen: Gen[Int] = Gen.const(1)

  def main(args: Array[String]): Unit = {
    println(
      Gen.custom[(Int, String)].make.unsafe()
    )

    println(
      Gen
        .custom[Option[Int]]
        .specifyConst(_.when[Some[Int]].value)(-1)
        .make
        .unsafe()
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
  }
}
