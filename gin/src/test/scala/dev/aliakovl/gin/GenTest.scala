package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{const, uglyString}

import scala.annotation.tailrec

sealed trait Lst[+A]
case object Nil extends Lst[Nothing]
case class Cons[A](head: A, tail: Lst[A]) extends Lst[A]

sealed trait MyClass
case class MyClass1(m: MyClass) extends MyClass
case class MyClass2(lst: Lst[Int], mc2field: String) extends MyClass
case class MyClass3() extends MyClass
case object MyClass4 extends MyClass

sealed trait KJH extends MyClass
case class K() extends KJH
case object H extends KJH

object GenTest {
  def main(args: Array[String]): Unit = {

    val r =
      Gen[MyClass]
        .specify(_.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field, uglyString(100))
        .specify(_.when[MyClass1].m.when[MyClass2].lst, const(Cons(3, Nil)))
        .specify(_.when[MyClass2].lst, const(Cons(4, Nil)))
        .random

    val rel = {
      val random$Int = implicitly[Random[Int]]
      val random$String = implicitly[Random[String]]
      def random$lst[A: Random]: Random[Lst[A]] = implicitly[Random[Lst[A]]]

      val MyClass$size = 4
      def MyClass$Trait = scala.util.Random.nextInt(MyClass$size)

      lazy val result: Random[MyClass] = Random(
        MyClass$Trait match {
          case 0 =>
            MyClass1(m = MyClass$Trait match {
              case 0 =>
                MyClass1(m = MyClass$Trait match {
                  case 0 => MyClass1(m = result.get())
                  case 1 =>
                    MyClass2(
                      lst = const(Cons(4, Nil)).get(),
                      mc2field = uglyString(100).get()
                    )
                  case 2 => MyClass3()
                  case 3 => MyClass4
                })
              case 1 =>
                MyClass2(lst = random$lst[Int].get(), mc2field = random$String.get())
              case 2 => MyClass3()
              case 3 => MyClass4
            })
          case 1 =>
            MyClass2(lst = const(Cons(4, Nil)).get(), mc2field = random$String.get())
          case 2 => MyClass3()
          case 3 => MyClass4
        }
      )
      result
    }

    @tailrec
    def loop: MyClass1 = rel.get() match {
      case a @ MyClass1(
            MyClass1(MyClass1(MyClass1(MyClass1(MyClass2(_, _)))))
          ) =>
        a
      case _ => loop
    }

    println(loop)

    val test = Gen[MyClass]
      .specify(_.when[MyClass1].m, const[MyClass](MyClass1(MyClass2(Nil, "wef"))))
      .random
      .get()
    println(test)
  }
}
