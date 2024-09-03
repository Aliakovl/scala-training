package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{const, oneOf, uglyString}

sealed trait Lst[+A]
case object LNil extends Lst[Nothing]
case class Cons[B](head: B, tail: Lst[B]) extends Lst[B]

sealed trait MyClass
case class MyClass1(m: MyClass) extends MyClass
case class MyClass2(lst: Lst[Int], mc2field: String = "lol", other: Lst[Long] = LNil) extends MyClass
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
        .specify(
          _.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field,
          uglyString(100)
        )
        .specify(_.when[MyClass1].m.when[MyClass2].lst, const(Cons(3, LNil)))
        .specify(_.when[MyClass2].lst, const(Cons(4, LNil)))
        .random

    println(mc.get())

    Random.many[List](10)(Gen[Lst[Int]].random).get().foreach(println)

    Gen[Unit].random.get()

    println(Gen[G].specify(_.int, oneOf(1, 5)).random.get().int)

    println(Gen[List[String]].specify(_.when[::[String]].head, const("wefwefef")).random.get())

    println(Gen[Lst[String]].specify(_.when[Cons[String]].tail.when[Cons[String]].head, uglyString(10)).random.get())

//    val a = Gen[Lst[String]]
//      .specify(_.when[Cons[String]].head, const("kek"))
//      .specify(_.when[Cons[String]].tail.when[Cons[String]].head, const("lol"))
//      .random

  }

}
