package dev.aliakovl.shapelessguide

import dev.aliakovl.gin._

object GenMain2 {

  sealed trait A
  case class B(i: Int) extends A
  case class C(i: Int) extends A

  implicit val sec: Int = 1234

  implicit def f[A: Gen]: Gen[Option[A]] = Gen.random[A].map(Some(_))

  def main(args: Array[String]): Unit = {
    Gen
      .random[List[Option[A]]]
      .many[List](10)
      .map(_.mkString("\n", "\n", "\n"))
      .tap(println)
      .run()

    Gen
      .random[Option[List[A]]]
      .many[List](10)
      .map(_.mkString("\n", "\n", "\n"))
      .tap(println)
      .run()

    Gen.custom[Lst[Int]].make

    Gen.random[Option[MyClass]].tap(println)
      .run()
    Gen.random[Option[MyClass]].tap(println)
      .run()
  }
}
