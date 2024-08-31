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

//    implicit lazy val mc1: Random[MyClass1] = Random(MyClass1(mc1.get()))
//    implicit lazy val mc2: Random[MyClass2] = Random(MyClass2(Nil, implicitly[Random[String]].get()))
//    implicit lazy val mc3: Random[MyClass3] = Random(MyClass3())
//    implicit lazy val mc4: Random[MyClass4.type] = Random(MyClass4)
//    implicit lazy val k: Random[K] = Random(K(implicitly[Random[String]].get()))
//    implicit lazy val j: Random[J] = Random(new J(implicitly[Random[String]].get()))
//    implicit lazy val h: Random[H.type] = Random(H)
//    implicit def lst[A: Random]: Random[Lst[A]] = Random(scala.util.Random.nextInt(2) match {
//      case 0 => Nil
//      case 1 => Cons(implicitly[Random[A]].get(), lst[A].get())
//    })

    lazy implicit val mc: String =
      Gen[MyClass]
        .specify(
          _.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field,
          uglyString(100)
        )
        .specify(_.when[MyClass1].m.when[MyClass2].lst, const(Cons(3, Nil)))
        .specify(_.when[MyClass2].lst, const(Cons(4, Nil)))
        .debug

    println(mc)

//    Random.many[List](100).make[MyClass].get().foreach(println)

    val rel = {
      val random$Int = implicitly[Random[Int]]
      val random$String = implicitly[Random[String]]
      val random$MyClass3 = Random(MyClass3())
      val random$MyClass4 = Random(MyClass4)
//      @tailrec
//      def random$lst[A: Random](acc: Lst[A]): Random[Lst[A]] = {
//        val MyClass$size = 2
//        def MyClass$Trait = scala.util.Random.nextInt(MyClass$size)
//        MyClass$Trait match {
//          case 0 => Random(acc)
//          case 1 => random$lst(Cons(implicitly[Random[A]].get(), acc))
//        }
//      }

      def random$nil: Random[Nil.type] = Random(Nil)

      def random$cons[A: Random]: Random[Cons[A]] = Random(
        Cons(implicitly[Random[A]].get(), random$lst.get())
      )

      def random$lst[A](implicit ev: Random[A]): Random[Lst[A]] = {
        val MyClass$size = 2
        def MyClass$Trait = scala.util.Random.nextInt(MyClass$size)
        Random(
          MyClass$Trait match {
            case 0 => random$nil.get()
            case 1 => random$cons.get()
          }
        )
      }

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
                  case 2 => random$MyClass3.get()
                  case 3 => random$MyClass4.get()
                })
              case 1 =>
                MyClass2(
                  lst = random$lst[Int].get(),
                  mc2field = random$String.get()
                )
              case 2 => random$MyClass3.get()
              case 3 => random$MyClass4.get()
            })
          case 1 =>
            MyClass2(
              lst = const(Cons(4, Nil)).get(),
              mc2field = random$String.get()
            )
          case 2 => random$MyClass3.get()
          case 3 => random$MyClass4.get()
        }
      )
      result
    }

    @tailrec
    def loop: MyClass1 = rel.get() match {
      case a @ MyClass1(
            MyClass1(MyClass1(MyClass1(MyClass1(MyClass2(_, _, _)))))
          ) =>
        a
      case _ => loop
    }

//    println(loop)

//    def wefwef[A] = Gen[Lst[A]].random

//    val test = Gen[MyClass]
//      .specify(_.when[MyClass1].m, const[MyClass](MyClass1(MyClass2(Nil, "wef"))))
//      .random
//      .get()
//    println(test)
  }

}
