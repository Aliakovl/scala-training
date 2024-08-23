package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{const, uglyString}

sealed trait Lst[+A]
case object Nil extends Lst[Nothing]
case class Cons[A](head: A, tail: Lst[A]) extends Lst[A]

sealed trait MyClass extends Product with Serializable
case class MyClass1(m: MyClass) extends MyClass
case class MyClass2(int: Int, mc2field: String) extends MyClass
case class MyClass3() extends MyClass
case object MyClass4 extends MyClass

object Main {
  def main(args: Array[String]): Unit = {

//    val r =
//      Gen[MyClass]
//        .specify[String](_.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field, uglyString(100))
//        .specify[Int](_.when[MyClass1].m.when[MyClass2].int, const(4))
//        .specify[Int](_.when[MyClass2].int, const(4))
//        .random

    val rel = {
      val random$Int = implicitly[Random[Int]]
      val random$String = implicitly[Random[String]]

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
                      int = random$Int.get(),
                      mc2field = uglyString(100).get()
                    )
                  case 2 => MyClass3()
                  case 3 => MyClass4
                })
              case 1 =>
                MyClass2(int = random$Int.get(), mc2field = random$String.get())
              case 2 => MyClass3()
              case 3 => MyClass4
            })
          case 1 =>
            MyClass2(int = const(4).get(), mc2field = random$String.get())
          case 2 => MyClass3()
          case 3 => MyClass4
        }
      )
      result
    }

    lazy val loop: MyClass1 = rel.get() match {
      case a @ MyClass1(
            MyClass1(MyClass1(MyClass1(MyClass1(MyClass2(_, _)))))
          ) =>
        a
      case _ => loop
    }

    println(loop)
  }
}
