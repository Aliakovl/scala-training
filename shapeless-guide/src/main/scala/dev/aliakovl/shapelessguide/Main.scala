package dev.aliakovl.shapelessguide

import dev.aliakovl.shapelessguide.examples.CsvEncoder.writeCsv
import dev.aliakovl.shapelessguide.examples._
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.Last
import shapeless.syntax.SingletonOps
import shapeless.syntax.singleton.mkSingletonOps

import scala.reflect.runtime.universe.reify

case class Bar(baz: Int, qux: String)
case class Foo(bar: Bar)

object Main {
  def getFieldName[K, V](value: FieldType[K, V])(implicit
      witness: Witness.Aux[K]
  ): K = witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V =
    value

  def main(args: Array[String]): Unit = {
    val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
    val encodedRepr = reprEncoder.encode("abc" :: 123 :: true :: HNil)
    val encodedIceCream =
      CsvEncoder[IceCream].encode(IceCream("abc", 123, inCone = true))
    println(encodedRepr)
    println(encodedIceCream)

    val iceCreams: List[IceCream] = List(
      IceCream("Sundae", 1, false),
      IceCream("Cornetto", 0, true),
      IceCream("Banana Split", 0, false)
    )
    println(writeCsv(iceCreams))

    val shapes: List[Shape] = List(
      Rectangle(3.0, 4.0),
      Circle(1.0)
    )

    println(writeCsv(shapes))

    CsvEncoder[Tree[Int]]

    println(CsvEncoder[Foo].encode(Foo(Bar(3, "wef"))))

    println(reify { CsvEncoder[Double] })

    val a = implicitly[Last[String :: Int :: HNil]].apply("abc" :: 123 :: HNil)

    val b = Last[String :: Int :: HNil].apply("abc" :: 123 :: HNil)

    val c = the[Last[String :: Int :: HNil]]

    val second1: Second.Aux[String :: Boolean :: Int :: HNil, Boolean] =
      Second[String :: Boolean :: Int :: HNil]
    val second2: Second.Aux[String :: Int :: Boolean :: HNil, Int] =
      Second[String :: Int :: Boolean :: HNil]

    val x = 42.narrow

    val xx: 42 = 42

    val someNumber = 123

    val numCherries =
      "numCherries" ->> someNumber

    trait Cherries

    val cherries123 = field[Cherries](123)

    val str = getFieldName(numCherries)
    println(str)
    println(getFieldValue(numCherries))

  }
}
