package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{oneOf, random, uglyString}

sealed trait Lst[+A]
case object LNil extends Lst[Nothing]
case class Cons[B](head: B, tail: Lst[B]) extends Lst[B]

sealed trait MyClass
case class MyClass1(m: MyClass) extends MyClass
case class MyClass2(
    lst: Lst[Int],
    mc2field: String = "lol",
    other: Lst[Long] = LNil
) extends MyClass
case class MyClass3() extends MyClass
case object MyClass4 extends MyClass

sealed abstract class KJH(val message: String) extends MyClass
case class K(override val message: String) extends KJH(message)
final class J(message: String) extends KJH(message)
case object H extends KJH("twert")

class G(val int: Int)

object GenTest {

  def main(args: Array[String]): Unit = {
    val mc: Random[MyClass] =
      Gen[MyClass]
        .specify(_.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field)(
          uglyString(100)
        )
        .specify(_.when[MyClass1].m.when[MyClass2].lst)(Cons(3, LNil))
        .specify(_.when[MyClass2].lst)(Cons(4, LNil))
        .random

    println(mc())

    Random.many[List](10)(Gen[Lst[Int]].random).apply().foreach(println)

    Gen[Unit].random()

    println(Gen[G].specify(_.int)(oneOf(1, 5)).random().int)

    println(
      Gen[List[String]]
        .specify(_.when[::[String]].head)("wefwefef")
        .random()
    )

    println(
      Gen[Lst[String]]
        .specify(_.when[Cons[String]].tail.when[Cons[String]].head)(
          uglyString(10)
        )
        .random()
    )

    val keklol = Gen[Lst[String]]
      .specify(_.when[Cons[String]].head)("kek")
      .specify(_.when[Cons[String]].tail.when[Cons[String]].head)("lol")
      .random

    println(Random.many[List](10)(keklol).apply())

    val f: Random[Lst[String]] = Gen[Lst[String]]
      .specify(_.when[Cons[String]].head)(random[String].map(_.toUpperCase))
      .specify(_.when[Cons[String]].tail)(random[Lst[String]])
      .random

    println(Random.many[List](10)(f).apply().mkString(", "))

    println(Random.many[List](10).make[Lst[String]].apply())

    println {
      (for {
        a <- Random.oneOf(1, 2)
        if a == 1
      } yield a).apply()
    }
    println(Random.many[List](10)(f).apply().mkString(", "))

    val t: Random[List[MyClass]] = Random.many[List](100) {
      Random.oneOf2(
        Gen[MyClass1].specify(_.m.when[MyClass2].mc2field)("wef").random,
        Gen[MyClass3].random
      )
    }

    println(t())

  }

}
