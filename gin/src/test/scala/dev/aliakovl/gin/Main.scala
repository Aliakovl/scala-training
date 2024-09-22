package dev.aliakovl.gin

import cats.{Functor, Monad}

object Main {
  def main(args: Array[String]): Unit = {
    println(
      Gen.custom[(Int, String)].make()
    )

    println(
      Gen
        .custom[Option[Int]]
        .specifyConst(_.when[Some[Int]].value)(-1)
        .make()
    )

    implicitly[Monad[Gen]].map(Gen.intGen)(_.toString).foreach(println)
    implicitly[Functor[Gen]].map(Gen.intGen)(_.toString).foreach(println)

    Gen.const(3).map(_.toString).foreach(println)

  }
}
