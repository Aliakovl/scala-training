package dev.aliakovl.shapelessguide

import cats.Monoid
import dev.aliakovl.shapelessguide.Migration.MigrationOps
import dev.aliakovl.shapelessguide.examples.CsvEncoder.writeCsv
import dev.aliakovl.shapelessguide.examples._
import dev.aliakovl.shapelessguide.examples.json.JsonEncoder
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.Last
import shapeless.syntax.singleton.mkSingletonOps

import scala.reflect.runtime.universe.reify

case class Bar(baz: Int, qux: String)
case class Foo(bar: Bar)

case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

case class IceCreamV2a(name: String, inCone: Boolean)

case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)

case class IceCreamV2c(
    name: String,
    inCone: Boolean,
    numCherries: Int,
    numWaffles: Int
)

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

    println(Witness[23].value)

    val iceCream = IceCream("Sundae", 1, false)

    val gen = LabelledGeneric[IceCream].to(iceCream)
    println(gen)

    println(JsonEncoder[IceCream].encode(iceCream))

    val shape: Shape = Circle(1.0)
    println(JsonEncoder[Shape].encode(shape))

    def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
      new Monoid[A] {
        def empty: A = zero
        def combine(x: A, y: A): A = add(x, y)
      }

    implicit val hnilMonoid: Monoid[HNil] =
      createMonoid[HNil](HNil)((_, _) => HNil)

    implicit def emptyHList[K <: Symbol, H, T <: HList](implicit
        hMonoid: Lazy[Monoid[H]],
        tMonoid: Monoid[T]
    ): Monoid[FieldType[K, H] :: T] =
      createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) { (x, y) =>
        field[K](hMonoid.value.combine(x.head, y.head)) :: tMonoid.combine(
          x.tail,
          y.tail
        )
      }

    println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2a])

    println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b])

    println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c])

  }
}
