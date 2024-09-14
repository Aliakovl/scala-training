package dev.aliakovl.other

import dev.aliakovl.gin._

sealed trait Lst[+A]
case object LNil extends Lst[Nothing]
case class Cons[B](head: B, tail: Lst[B]) extends Lst[B]

sealed trait MyClass
case class MyClass1(m: MyClass) extends MyClass
case class MyClass2(lst: Lst[Int], mc2field: String = "lol", other: Lst[Long] = LNil) extends MyClass
case class MyClass3(t: 2) extends MyClass
case object MyClass4 extends MyClass

sealed abstract class KJH(val message: String) extends MyClass
case class K(override val message: String) extends KJH(message)
final class J(message: String) extends KJH(message)
case object H extends KJH("twert")

class G(val int: Int)

case class Talk(int: Int)(string: String, gerg: List[Int])(implicit wefwef: Option[Long]) {
  override def toString: String = s"Talk($int)($string, $gerg)($wefwef)"
}

object GenTest {

  implicit val mc2: Random[MyClass2] = Gen[MyClass2].specifyConst(_.mc2field)("LLLLLLL").random
  implicit val mc1: Random[MyClass1] = Gen[MyClass1].specifyConst(_.m)(H).random

  implicit val str: Random[String] = Random.const("CONST")
  implicit val int: Random[Int] = Random.oneOf(1, 2, 3)

  def main(args: Array[String]): Unit = {
    val a: MyClass2 = Random.random[MyClass2].apply()
    val b: MyClass2 = Gen[MyClass2].random()
    println(a)
    println(b)

    println(Gen[List[MyClass1]].random())

    val mc: Random[MyClass] =
      Gen[MyClass]
        .specify(_.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field)(
          Random.uglyString(100)
        )
        .specifyConst(_.when[MyClass1].m.when[MyClass2].lst)(Cons(3, LNil))
        .specifyConst(_.when[MyClass2].lst)(Cons(4, LNil))
        .random

    println(mc())

    Random.many[List](10)(Gen[Lst[Int]].random).apply().foreach(println)

    Gen[Unit].random()

    println(Gen[G].specify(_.int)(Random.oneOf(1, 5)).random().int)

    println(
      Gen[List[String]]
        .specifyConst(_.when[::[String]].head)("wefwefef")
        .random()
    )

    println(
      Gen[Lst[String]]
        .specify(_.when[Cons[String]].tail.when[Cons[String]].head)(
          Random.uglyString(10)
        )
        .random()
    )

    val keklol = Gen[Lst[String]]
      .specifyConst(_.when[Cons[String]].head)("kek")
      .specifyConst(_.when[Cons[String]].tail.when[Cons[String]].head)("lol")
      .random

    println(Random.many[List](10)(keklol).apply())

    val f: Random[Lst[String]] = Gen[Lst[String]]
      .specify(_.when[Cons[String]].head)(Random.random[String].map(_.toUpperCase))
      .specify(_.when[Cons[String]].tail)(Random.random[Lst[String]])
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

    val tt: Random[MyClass] = Random.oneOfRandom(
      Gen[MyClass1].specifyConst(_.m.when[MyClass2].mc2field)("wef").random,
      Gen[MyClass3].random
    )

    println(Random.many[List](3)(tt).apply())

    val y: (Int, MyClass4.type) = (4, MyClass4)

    val r = Random.random[MyClass3]
    println(Random.many[List](10)(r).apply())

    println(Random.random[y.type].apply())

    val rp: Random[String] = Random.oneOfRandom[String](
      Gen["RRRRR"].random,
      Gen["PPPPPP"].random
    )

    val rppp = Random.oneOf[String].make["RRRRR", "PPPPPP"]

    println(Random.many[List](10)(rp).apply())
    println(Random.many[List](10)(rppp).apply())

    val cr: Random[MyClass] = Random.oneOf[MyClass].make[MyClass1, MyClass2, MyClass3]

    println(cr())

    println(Random.many[List](10)(Random.oneOf[Any].make[String, Int]).apply())

    val g = Gen[MyClass]
//      .specify(_.when[MyClass1].m)(Random.random[MyClass])
      .specifyConst(_.when[MyClass1].m.when[MyClass2].mc2field)("lol")
      .specify(_.when[MyClass1].m.when[MyClass2].lst)(Random.random[Lst[Int]])
      .random

    println(g())

    println(Gen[Talk].random())
  }

}
