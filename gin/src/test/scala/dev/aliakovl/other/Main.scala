package dev.aliakovl.other

import dev.aliakovl.gin.Random

object Main {
  sealed trait MyClass extends Product with Serializable
  case class MyClass1(m: MyClass) extends MyClass
  case class MyClass2(int: Int, mc2field: String) extends MyClass
  case class MyClass3() extends MyClass
  case object MyClass4 extends MyClass

  sealed trait SubClass extends MyClass
  case class One() extends SubClass

  def main(args: Array[String]): Unit = {
    println(Random.random[Map[String, List[Option[MyClass]]]].apply())
    println(Random.many[List](3)(Random.random[String]).apply())
    println(
      Random.many2[Map](3)(Random.random[String], Random.random[Int]).apply()
    )
    println(Random.many2[Map](3).make[String, Int].apply())
    println(Random.random[Either[String, Int]].apply())
    val a: Random[MyClass] = Random.oneOf[MyClass].make[MyClass2, MyClass1]
    println(Random.many[List](10)(a).apply())
    val b: List[MyClass] = Random
      .many[List](4000)(
        Random.oneOf[MyClass].make[MyClass1, MyClass2, MyClass3, SubClass]
      )
      .apply()
    println(b.count(_.isInstanceOf[MyClass1]))
    println(b.count(_.isInstanceOf[MyClass2]))
    println(b.count(_.isInstanceOf[MyClass3]))
    println(b.count(_.isInstanceOf[One]))
    println(Random.many[List](10).make[Int].apply())

    println(Random.uglyString(10)())
    println(Random.string(10).apply())
    println(Random.alphanumeric(10).apply())
    println(Random.many[List](10)(Random.oneOf(3, 4, 5, 7)).apply())

    println(Random.oneOfRandom(Random.apply({print("first - "); 1}), Random.apply({print("second - "); 2})).apply())
  }
}
