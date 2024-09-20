package dev.aliakovl.other

import dev.aliakovl.gin.Gen

object Main {
  sealed trait MyClass extends Product with Serializable
  case class MyClass1(m: MyClass) extends MyClass
  case class MyClass2(int: Int, mc2field: String) extends MyClass
  case class MyClass3() extends MyClass
  case object MyClass4 extends MyClass

  sealed trait SubClass extends MyClass
  case class One() extends SubClass

  def main(args: Array[String]): Unit = {
    println(Gen.random[Map[String, List[Option[MyClass]]]].apply())
    println(Gen.many[List](3)(Gen.random[String]).apply())
    println(
      Gen.many2[Map](3)(Gen.random[String], Gen.random[Int]).apply()
    )
    println(Gen.many2[Map](3).make[String, Int].apply())
    println(Gen.random[Either[String, Int]].apply())
    val a: Gen[MyClass] = Gen.oneOf[MyClass].make[MyClass2, MyClass1]
    println(Gen.many[List](10)(a).apply())
    val b: List[MyClass] = Gen
      .many[List](4000)(
        Gen.oneOf[MyClass].make[MyClass1, MyClass2, MyClass3, SubClass]
      )
      .apply()
    println(b.count(_.isInstanceOf[MyClass1]))
    println(b.count(_.isInstanceOf[MyClass2]))
    println(b.count(_.isInstanceOf[MyClass3]))
    println(b.count(_.isInstanceOf[One]))
    println(Gen.many[List](10).make[Int].apply())
    println(Gen.random[Int].many[List](10).apply())

    println(Gen.uglyString(10)())
    println(Gen.string(10)())
    println(Gen.alphanumeric(10)())
    println(Gen.many[List](10)(Gen.oneOf(3, 4, 5, 7)).apply())

    println(Gen.oneOfGen(Gen({print("first - "); 1}), Gen({print("second - "); 2})).apply())
  }
}
