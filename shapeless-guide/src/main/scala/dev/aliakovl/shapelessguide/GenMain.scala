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

//  Gen
//    .custom[Opt[Int]]
//    .specifyConst(_.when[Maybe].value)(34)
//    .make
//    .many[List](10)
//    .map(_.mkString("\n"))
//    .tap(println)
//    .run()
//
//  Gen
//    .custom[Option[Int]]
//    .specifyConst(_.when[Some].value)(34)
//    .make
//    .many[List](10)
//    .map(_.mkString("\n"))
//    .tap(println)
//    .run()

  Gen
    .custom[List[String]]
    .make
    .many[List](10)
    .map(_.mkString("\n"))
    .tap(println)
    .run()

//  Gen
//    .custom[List[String]]
//    .specifyConst(_.when[::[String]].head)("15")
//    .make
//    .many[List](10)
//    .tap(println)
//    .run()
//
//  println
//
//  Gen
//    .custom[List[String]]
//    .specifyConst(_.when[::].head)("15")
//    .make
//    .many[List](10)
//    .tap(println)
//    .run()

}
