package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{const, uglyString}

import scala.annotation.tailrec

sealed trait Lst[+A]
case object Nil extends Lst[Nothing]
case class Cons[A](head: A, tail: Lst[A]) extends Lst[A]

sealed trait MyClass
case class MyClass1(m: MyClass) extends MyClass
case class MyClass2(lst: Lst[Int], mc2field: String = "lol", other: Lst[Long] = Nil) extends MyClass
case class MyClass3() extends MyClass
case object MyClass4 extends MyClass

sealed abstract class KJH(val message: String) extends MyClass
case class K(override val message: String) extends KJH(message)
final class J(message: String) extends KJH(message)
case object H extends KJH("twert")

object GenTest {

  def main(args: Array[String]): Unit = {

    implicit lazy val mc1: Random[MyClass1] = Random(MyClass1(mc.get()))
    implicit lazy val mc2: Random[MyClass2] = Random(MyClass2(Nil, implicitly[Random[String]].get()))
    implicit lazy val mc3: Random[MyClass3] = Random(MyClass3())
    implicit lazy val mc4: Random[MyClass4.type] = Random(MyClass4)
    implicit lazy val k: Random[K] = Random(K(implicitly[Random[String]].get()))
    implicit lazy val j: Random[J] = Random(new J(implicitly[Random[String]].get()))
    implicit lazy val h: Random[H.type] = Random(H)
    implicit def lst[A: Random]: Random[Lst[A]] = Random(scala.util.Random.nextInt(2) match {
      case 0 => Nil
      case 1 => Cons(implicitly[Random[A]].get(), lst[A].get())
    })

    lazy implicit val mc: Random[MyClass] =
      Gen[MyClass]
        .specify(
          _.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field,
          uglyString(100)
        )
        .specify(_.when[MyClass1].m.when[MyClass2].lst, const(Cons(3, Nil)))
        .specify(_.when[MyClass2].lst, const(Cons(4, Nil)))
        .random

    println(mc.get())

    println(Gen[String].random.get())

  }

}
