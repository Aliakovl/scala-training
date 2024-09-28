package dev.aliakovl.shapelessguide

import dev.aliakovl.gin.{Gen, GenWhen}

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

  Gen
    .custom[Talk]
    .specifyConst(_.mc.when[Some[MyClass]].value.when[MyClass2])(
      MyClass2(lst = Cons(6, Cons(6, Cons(6, LNil))))()
    )
    .make
    .many[List](100)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen
    .custom[Talk]
    .specifyConst(_.mc.when[Some[MyClass2]].value)(
      MyClass2(Cons(6, Cons(6, Cons(6, LNil))))()
    )
    .make
    .map(_.mc)
    .many[List](100)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen
    .custom[Talk]
    .specifyConst(_.mc.when[Some[MyClass2]])(
      Some(MyClass2(Cons(6, Cons(6, Cons(6, LNil))))())
    )
    .make
    .map(_.mc)
    .many[List](100)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen
    .custom[Option[MyClass]]
    .specifyConst(_.when[Some[MyClass2]])(
      Some(MyClass2(Cons(6, Cons(6, Cons(6, LNil))))())
    )
    .make
    .many[List](100)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

  Gen
    .custom[Opt[Clazz]]
    .specify(_.when[Maybe[Clazz]])(
      Gen.custom[Clazz].exclude[A].make.map(Maybe(_))
    )
    .exclude[Noth[Clazz]]
    .make
    .many[List](10000)
    .map(_.count {
      case Maybe(A()) => false
      case Maybe(B()) => true
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
    .exclude[Wrapper[LolB]]
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
    .specifyConst(_.when[Some[Left[String, Int]]])(Some(Left("left")))
    .specifyConst(_.when[Some[Right[String, Int]]].value.value)(4)
    .exclude[Some[Left[String, Int]]]
    .make
    .many[List](100)
    .map(_.mkString("\n"))
    .tap(println)
    .runWithSeed(123)

}