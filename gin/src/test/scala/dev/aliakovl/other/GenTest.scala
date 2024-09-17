package dev.aliakovl.other

import dev.aliakovl.gin._

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
case class MyClass3(t: 2) extends MyClass
case object MyClass4 extends MyClass

sealed abstract class KJH(val message: String) extends MyClass
case class K(override val message: String) extends KJH(message)
final class J(message: String) extends KJH(message)
case object H extends KJH("twert")

class G(val int: Int)

case class Talk(int: Int)(val string: String, val gerg: List[Int])(
    wefwef: Option[Long]
) {
  override def toString: String = s"Talk($int)($string, $gerg)($wefwef)"
}

object GenTest {

  implicit val mc2: Gen[MyClass2] =
    Gen.custom[MyClass2].specifyConst(_.mc2field)("LLLLLLL").random
  implicit val mc1: Gen[MyClass1] =
    Gen.custom[MyClass1].specifyConst(_.m)(H).random

  implicit val str: Gen[String] = Gen.const("CONST")
  implicit val int: Gen[Int] = Gen.oneOf(1, 2, 3)

  def main(args: Array[String]): Unit = {
    val a: MyClass2 = Gen.random[MyClass2].apply()
    val b: MyClass2 = Gen.custom[MyClass2].random()
    println(a)
    println(b)

    println(Gen.custom[List[MyClass1]].random())

    val mc: Gen[MyClass] =
      Gen
        .custom[MyClass]
        .specify(_.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field)(
          Gen.uglyString(100)
        )
        .specifyConst(_.when[MyClass1].m.when[MyClass2].lst)(Cons(3, LNil))
        .specifyConst(_.when[MyClass2].lst)(Cons(4, LNil))
        .random

    println(mc())

    Gen
      .many[List](10)(Gen.custom[Lst[Int]].random)
      .apply()
      .foreach(println)

    Gen.custom[Unit].random()

    println(Gen.custom[G].specify(_.int)(Gen.oneOf(1, 5)).random().int)

    println(
      Gen
        .custom[List[String]]
        .specifyConst(_.when[::[String]].head)("wefwefef")
        .random()
    )

    println(
      Gen
        .custom[Lst[String]]
        .specify(_.when[Cons[String]].tail.when[Cons[String]].head)(
          Gen.uglyString(10)
        )
        .random()
    )

    val keklol = Gen
      .custom[Lst[String]]
      .specifyConst(_.when[Cons[String]].head)("kek")
      .specifyConst(_.when[Cons[String]].tail.when[Cons[String]].head)("lol")
      .random

    println(Gen.many[List](10)(keklol).apply())

    val f: Gen[Lst[String]] = Gen
      .custom[Lst[String]]
      .specify(_.when[Cons[String]].head)(
        Gen.random[String].map(_.toUpperCase)
      )
      .specify(_.when[Cons[String]].tail)(Gen.random[Lst[String]])
      .random

    println(Gen.many[List](10)(f).apply().mkString(", "))

    println(Gen.many[List](10).make[Lst[String]].apply())

    println(Gen.many[List](10)(f).apply().mkString(", "))

    val tt: Gen[MyClass] = Gen.oneOfGen(
      Gen
        .custom[MyClass1]
        .specifyConst(_.m.when[MyClass2].mc2field)("wef")
        .random,
      Gen.custom[MyClass3].random
    )

    println(Gen.many[List](3)(tt).apply())

    val y: (Int, MyClass4.type) = (4, MyClass4)

    val r = Gen.random[MyClass3]
    println(Gen.many[List](10)(r).apply())

    println(Gen.random[y.type].apply())

    val rp: Gen[String] = Gen.oneOfGen[String](
      Gen.custom["RRRRR"].random,
      Gen.custom["PPPPPP"].random
    )

    val rppp = Gen.oneOf[String].make["RRRRR", "PPPPPP"]

    println(Gen.many[List](10)(rp).apply())
    println(Gen.many[List](10)(rppp).apply())

    val cr: Gen[MyClass] =
      Gen.oneOf[MyClass].make[MyClass1, MyClass2, MyClass3]

    println(cr())

    println(Gen.many[List](10)(Gen.oneOf[Any].make[String, Int]).apply())

    val g = Gen
      .custom[MyClass]
//      .specify(_.when[MyClass1].m)(Random.random[MyClass])
      .specifyConst(_.when[MyClass2])(MyClass2(null, null, null))
      .random

    println(g())

    println(Gen.custom[Talk].random())

    println(
      Gen
        .custom[Talk]
        .specifyConst[List[Int]](_.gerg)(List(3, 2, 1))
        .random()
    )

    println(
      Gen
        .custom[Talk]
        .specifyConst(f => f.gerg)(List(9, 8, 7))
        .random()
    )

    Gen
      .custom[MyClass]
      .specifyConst(_.when[KJH])(K("YES"))
      .random
      .many[List](10)
      .foreach(println)

  }

}
