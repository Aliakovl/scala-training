package dev.aliakovl.shapelessguide

import dev.aliakovl.gin._

import scala.language.existentials

sealed trait Lst[+A]
case object LNil extends Lst[Nothing]
case class Cons[B](head: B, tail: Lst[B]) extends Lst[B]

sealed trait MyClass
case class MyClass1(m: MyClass = MyClass4) extends MyClass
case class MyClass2(lst: Lst[Int], mc2field: String = "lol-default")(
    other: Lst[Long] = Cons(mc2field.length.toLong, LNil)
) extends MyClass {
  override def toString: String = s"MyClass2($lst, $mc2field)($other)"
}
case class MyClass3(t: 2) extends MyClass
case object MyClass4 extends MyClass

sealed abstract class KJH(val message: String) extends MyClass
case class K(override val message: String) extends KJH(message)
final class J(message: String) extends KJH(message) {
  override def toString: String = s"J($message)"
}
case object H extends KJH("twert")

class G(val int: Int) {
  override def toString: String = s"G($int)"
}

case class Talk(int: Int)(
    val string: String,
    val gerg: List[Int],
    val mc: Option[MyClass] = None
)(implicit
    val wefwef: Option[Long],
    sec: Int
) {
  override def toString: String =
    s"Talk($int)($string, $gerg, $mc)($wefwef, $sec)"
}

sealed abstract class Clazz
case class A() extends Clazz
case class B() extends Clazz
case class C() extends Clazz

sealed trait Opt[A]
case class Noth[A]() extends Opt[A]
case class Maybe[A](value: A) extends Opt[A]

sealed trait Wrapper[+A]
case class WrapperImpl[+A](field: Int) extends Wrapper[A]

object GenMain extends App {
  implicit val qweffw: Option[Long] = Some(3L)
  implicit val fqfqfqwef: Int = 1235414235

  sealed trait TypeClass[+F[_]]
  case class TypeClassInst[F[_]]() extends TypeClass[F]
  case class TypeClassLol[A <: Int](l: List[A]) extends TypeClass[List]

  Gen.custom[Opt[Int]]
    .specifyConst(_.when1[Maybe].value)(34)
    .make
    .many[List](10)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen.custom[Option[Int]]
    .specifyConst(_.when1[Some].value)(34)
    .make
    .many[List](10)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen
    .custom[Talk]
    .specifyConst(_.mc.when1[Some].value.when[MyClass2])(
      MyClass2(lst = Cons(6, Cons(6, Cons(6, LNil))))()
    )
    .make
    .many[List](10)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen
    .custom[Talk]
    .specifyConst(_.mc.when1[Some].value.when[MyClass2])(
      MyClass2(Cons(6, Cons(6, Cons(6, LNil))))()
    )
    .make
    .map(_.mc)
    .many[List](100)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen
    .custom[Opt[Clazz]]
    .exclude(_.when1[Maybe].value.when[A])
    .exclude(_.when1[Maybe].value.when[B])
    .exclude(_.when1[Noth])
    .make
    .many[List](10000)
    .map(_.count {
      case Maybe(A()) => false
      case Maybe(B()) => false
      case Maybe(C()) => true
      case Noth()     => false
    })
    .tap(println)
    .run()

  sealed trait Lol
  case class LolA() extends Lol
  case class LolB() extends Lol

  Gen
    .custom[Wrapper[Lol]]
    .specify(_.when[Wrapper[LolA]])(Gen.apply{_ => print("LolA!\n"); WrapperImpl(3)})
    .exclude(_.when[Wrapper[LolB]])
    .make
    .many[List](10)
    .tap(println)
    .run()

  Gen
    .custom[Option[Lol]]
    .make
    .many[List](10000)
    .map(_.count(_.isEmpty))
    .tap(println)
    .runWithSeed(123)

  Gen
    .custom[Opt[Lol]]
    .make
    .many[List](10000)
    .map(_.count {
      case Noth() => true
      case Maybe(value) => false
    })
    .tap(println)
    .runWithSeed(123)

  def f[A: Gen]: Gen[Opt[A]] = {
    Gen.custom[Opt[A]].make
  }

  Gen
    .custom[Opt[Either[String, Int]]]
    .make
    .many[List](10000)
    .map(_.count {
      case Noth()       => true
      case Maybe(value) => false
    })
    .tap(println)
    .runWithSeed(123)

  f[Either[String, Int]]
    .many[List](10000)
    .map(_.count {
      case Noth()       => true
      case Maybe(value) => false
    })
    .tap(println)
    .runWithSeed(123)

  def ff[A: Gen]: Gen[Option[A]] = {
    Gen.custom[Option[A]].make
  }

  ff[Either[String, Int]]
    .many[List](10000)
    .map(_.count(_.isEmpty))
    .tap(println)
    .runWithSeed(123)

  implicit lazy val wef: Gen[Either[String, Int]] =
    Gen.custom[Either[String, Int]].make

  wef.run()

  Gen.custom[Option[Int]]
    .specifyConst(_.when[Some[Int]].when[Some[Int]].when[Some[Int]].when[Some[Int]].value)(3)
    .make

  Gen
    .custom[Option[Either[String, Int]]]
    .specifyConst(_.when1[Some].value.when[Right[String, Int]].value)(4)
    .exclude(_.when1[Some].value.when[Left[String, Int]])
    .make
    .many[List](100)
    .map(_.mkString("\n"))
    .tap(println)
    .runWithSeed(123)

  sealed trait Colour
  case class Blue() extends Colour
  case class Red() extends Colour

  case class BigType[A, B](a: A, b: B)

  Gen
    .custom[BigType[Int, Option[Colour]]]
        .exclude(_.b.when1[Some].value.when[Red])
    .make
    .many[List](10000)
    .map(_.count {
      case BigType(_, None) => true
      case _ => false
    })
    .tap(println)
    .run()

  Gen.custom[List[Option[Int]]]
    .make
    .many[List](10)
    .map(_.mkString("\n", "\n", "\n"))
    .tap(println)
    .run()

  Gen
    .custom[Option[Either[String, Int]]]
    .specifyConst(
      _.when1[Some].value.when2[Right].value
    )(4)
    .exclude(_.when1[Some].value.when[Left[String, Int]])
    .make
    .many[List](2)
    .map(_.mkString("\n"))
    .tap(println)
    .runWithSeed(123)

  sealed trait MyEither[L, R]
  case class MyLeft[L, R](value: L) extends MyEither[L, R]
  case class MyRight[R, L](value: R) extends MyEither[L, R]

  Gen.custom[MyEither[String, Int]]
    .specifyConst(_.when[MyLeft[String, Int]].value)("wefwef")
    .specifyConst(_.when[MyRight[Int, String]].value)(1234)
    .make
    .many[List](10)
    .tap(println)
    .run()

}
