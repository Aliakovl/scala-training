package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{const, random, uglyString}
import dev.aliakovl.gin._

sealed trait Lst[+A]
case object Nil extends Lst[Nothing]
case class Cons[A](head: A, tail: Lst[A]) extends Lst[A]

object Main {
  def main(args: Array[String]): Unit = {

//    val myClass: Random[MyClass2] = Random.random[MyClass2]

//    val a = RandomTransformer.help[MyClass2, String](
//      _.mc2field,
//      random[MyClass2],
//      uglyString(100)
//    )
//
//    println(a)
//
//    val wqef = RandomTransformer.specify[MyClass2, String](
//      _.mc2field,
//      random[MyClass2],
//      uglyString(5)
//    )
//    println(wqef.get())

//    println(ru.showRaw(ru.reify(random[MyClass2]).tree))

    val r =
      Gen[MyClass1]
        .specify[String](_.m.mc2field, uglyString(100))
        .specify[Int](_.m.int, const(4))
        .random

    val rel = {
      val wefqwef = implicitly[Random[Int]]
      val _qwef = implicitly[Random[String]]

      val res = Random(
        new MyClass1(
          m = new MyClass2(
            int = wefqwef.get(),
            mc2field = _qwef.get()
          )
        )
      )

      res
    }

//    val c = Gen[MyClass]
//      .specify(_.when[MyClass1].m.int, const(3))
//      .random

    val tc = {

      val mc2i = implicitly[Random[Int]]
      val mc2m = implicitly[Random[String]]

      val res = List(
        Random(
          new MyClass1(
            m = new MyClass2(
              int = const(3).get(),
              mc2field = mc2m.get()
            )
          )
        ),
        Random(
          new MyClass2(
            int = mc2i.get(),
            mc2field = mc2m.get()
          )
        ),
        Random(
          new MyClass3()
        ),
        Random(
          MyClass4
        )
      )

      Random.oneOfRandom(res: _*)
    }

//    val listR = Gen[Lst[Int]]
//      .specify(_.when[Cons[Int]].head, const(4))
//      .random

    val listReal: Random[Lst[Int]] = {
      val int = implicitly[Random[Int]]

      lazy val nilR: Random[Lst[Int]] = Random(Nil)
      lazy val lstR: Random[Lst[Int]] = Random(
        Cons(int.get(), res.get())
      )

      lazy val res: Random[Lst[Int]] = Random.oneOfRandom(
        nilR,
        lstR
      )

      res
    }

    println(r.get())
    println(rel.get())
    println(tc.get())
    println(listReal.get())

  }
}
