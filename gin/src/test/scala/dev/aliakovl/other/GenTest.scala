package dev.aliakovl.other

import cats.implicits.toTraverseOps
import dev.aliakovl.gin._

sealed trait Lst[+A]
case object LNil extends Lst[Nothing]
case class Cons[B](head: B, tail: Lst[B]) extends Lst[B]

sealed trait MyClass
case class MyClass1(m: MyClass) extends MyClass
case class MyClass2(lst: Lst[Int], mc2field: String = "lol-default")(
    other: Lst[Long] = Cons(mc2field.length.toLong, LNil)
) extends MyClass {
  override def toString: String = s"MyClass2($lst, $mc2field)($other)"
}
case class MyClass3(t: 2) extends MyClass
case object MyClass4 extends MyClass

sealed abstract class KJH(val message: String) extends MyClass
case class K(override val message: String) extends KJH(message)
final class J(message: String) extends KJH(message)
case object H extends KJH("twert")

class G(val int: Int)

case class Talk(int: Int)(val string: String, val gerg: List[Int])(implicit
    val wefwef: Option[Long],
    sec: Int
) {
  override def toString: String = s"Talk($int)($string, $gerg)($wefwef, $sec)"
}

class TestClass(val g: Int) {
  override def toString: String = s"TestClass($g)"
}
class TestClass2(private val t: String, val s: Int) extends TestClass(3) {
  override def toString: String = s"TestClass2($t)($g)($s)"
}

class T(a: Int, b: String) {
  override def toString: String = s"T($a, $b)"
}

object T {
  def apply(str: String): Option[T] =
    Option.when(str.nonEmpty) {
      new T(str.length, str)
    }
}

object GenTest {

  implicit val ol: Option[Long] = Some(11111111111L)

  implicit val mc2: Gen[MyClass2] =
    Gen.custom[MyClass2].specifyConst(_.mc2field)("LLLLLLL").make
  implicit val mc1: Gen[MyClass1] =
    Gen.custom[MyClass1].specifyConst(_.m)(H).make

  implicit val str: Gen[String] = Gen.between(0, 5).flatMap(Gen.alphanumeric)
  implicit val int: Gen[Int] = Gen.oneOf(1, 2, 3)

  val y: (Int, MyClass4.type) = (4, MyClass4)

  implicit val sec: Int = 1234

  val gens = Seq[Gen[Any]](
    Gen.random[MyClass2],
    Gen.custom[MyClass2].make,
    Gen.custom[List[MyClass1]].make,
    Gen
      .custom[MyClass]
      .specify(_.when[MyClass1].m.when[MyClass1].m.when[MyClass2].mc2field)(
        Gen.uglyString(100)
      )
      .specifyConst(_.when[MyClass1].m.when[MyClass2].lst)(Cons(3, LNil))
      .specifyConst(_.when[MyClass2].lst)(Cons(4, LNil))
      .make,
    Gen.custom[Lst[Int]].make.many[List](10),
    Gen.custom[Unit].make,
    Gen.custom[G].specify(_.int)(Gen.oneOf(1, 5)).make,
    Gen
      .custom[List[String]]
      .specifyConst(_.when[::[String]].head)("wefwefef")
      .make,
    Gen
      .custom[Lst[String]]
      .specify(_.when[Cons[String]].tail.when[Cons[String]].head)(
        Gen.uglyString(10)
      )
      .make,
    Gen
      .custom[Lst[String]]
      .specifyConst(_.when[Cons[String]].head)("kek")
      .specifyConst(_.when[Cons[String]].tail.when[Cons[String]].head)(
        "lol"
      )
      .make
      .many[List](10),
    Gen
      .custom[Lst[String]]
      .specify(_.when[Cons[String]].head)(
        Gen.random[String].map(_.toUpperCase)
      )
      .specify(_.when[Cons[String]].tail)(Gen.random[Lst[String]])
      .make
      .many[List](10)
      .map(_.mkString(", ")),
    Gen.random[Lst[String]].many[List](10),
    Gen
      .oneOfGen(
        Gen
          .custom[MyClass1]
          .specifyConst(_.m.when[MyClass2].mc2field)("wef")
          .make,
        Gen.custom[MyClass3].make
      )
      .many[List](3),
    Gen.random[MyClass3].many[List](10),
    Gen.random[y.type],
    Gen
      .oneOfGen[String](
        Gen.custom["RRRRR"].make,
        Gen.custom["PPPPPP"].make
      )
      .many[List](10),
    Gen.oneOf[String].make["RRRRR", "PPPPPP"].many[List](10),
    Gen.oneOf[MyClass].make[MyClass1, MyClass2, MyClass3],
    Gen.oneOf[Any].make[String, Int].many[List](10),
    Gen
      .custom[MyClass]
      .specify(_.when[MyClass1].m)(Gen.random[MyClass4.type])
      .specifyConst(_.when[MyClass2])(MyClass2(null, null)(null))
      .make,
    Gen.custom[Talk].make,
    Gen
      .custom[Talk]
      .specifyConst[List[Int]](_.gerg)(List(3, 2, 1))
      .specifyConst(_.wefwef)(Some(0))
      .make,
    Gen
      .custom[Talk]
      .specifyConst(f => f.gerg)(List(9, 8, 7))
      .make,
    Gen
      .custom[MyClass]
      .specifyConst(_.when[KJH])(K("YES"))
      .make
      .many[List](10),
    Gen.custom[MyClass4.type].make,
    Gen
      .custom[MyClass1]
      .useDefault(_.m.when[MyClass2].mc2field)
      .make,
    Gen
      .custom[MyClass2]
      .useDefault(_.mc2field)
      .useDefault(_.arg("other"))
      .make,
    Gen.fromFunction { lst: Lst[Int] => MyClass2(lst)() },
    Gen
      .custom[TestClass]
      .specifyConst(_.g)(1000)
      .make,
    Gen
      .custom[TestClass2]
      .specifyConst(_.arg("t"))("I am private")
      .make,
    Gen.fromFunction { new TestClass2("I am private", _) },
    Gen
      .fromFunction {
        s: String =>
          s.length
      },
    Gen.fromFunction(new T(2134, _)),
    Gen.fromFunction(T.apply _).many[List](3),
    Gen.custom[T].specifyConst(_.arg[Int]("a"))(2134).make,
    Gen.fromFunction(
      Some(
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String,
        _: String
      )
    )
  )

  def main(args: Array[String]): Unit = gens.sequence
    .foreach(
      _.zipWithIndex
        .map { case (value, id) =>
          s"$id\t-> $value"
        }
        .foreach(println)
    )

}
