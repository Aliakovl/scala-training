package dev.aliakovl.other

import cats.Traverse
import cats.implicits.toTraverseOps
import dev.aliakovl.gin._

object Main {
  sealed trait MyClass extends Product with Serializable
  case class MyClass1(m: MyClass) extends MyClass
  case class MyClass2(int: Int, mc2field: String) extends MyClass
  case class MyClass3() extends MyClass
  case object MyClass4 extends MyClass

  sealed trait SubClass extends MyClass
  case class One() extends SubClass
s
  val gens = Seq[Gen[Any]](
    Gen.random[Map[String, List[Option[MyClass]]]],
    Gen.random[String].many[List](3),
    Traverse[List].sequence(
      List(Gen.random[String], Gen.random[String], Gen.random[String])
    ),
    Gen.product(Gen.random[String], Gen.random[Int]).toMap[String, Int](3),
    Gen.random[(String, Int)].toMap[String, Int](3),
    Gen.random[Either[String, Int]],
    Gen.one[MyClass].of[MyClass2, MyClass1].many[List](10),
    Gen
      .one[MyClass]
      .of[MyClass1, MyClass2, MyClass3, SubClass]
      .many[List](4000)
      .map { list =>
        (
          list.count(_.isInstanceOf[MyClass1]),
          list.count(_.isInstanceOf[MyClass2]),
          list.count(_.isInstanceOf[MyClass3]),
          list.count(_.isInstanceOf[MyClass4.type]),
          list.count(_.isInstanceOf[One])
        )
      },
    Gen.random[Int].many[List](10),
    Gen.uglyString(10),
    Gen.string(10),
    Gen.alphanumeric(10),
    Gen.oneOf(3, 4, 5, 7).many[List](10)
  )

  def main(args: Array[String]): Unit = gens.sequence
    .tap(
      _.zipWithIndex
        .map { case (value, id) =>
          s"$id\t-> $value"
        }
        .foreach(println)
    ).runWithSeed(11)
}
