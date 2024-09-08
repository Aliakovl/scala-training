package dev.aliakovl.gin

import Random._

object Main {
  sealed trait MyClass extends Product with Serializable
  case class MyClass1(m: MyClass) extends MyClass
  case class MyClass2(int: Int, mc2field: String) extends MyClass
  case class MyClass3() extends MyClass
  case object MyClass4 extends MyClass

  sealed trait SubClass extends MyClass
  case class One() extends SubClass

  def main(args: Array[String]): Unit = {
    println(random[Map[String, List[Option[MyClass]]]].apply())
    println(many[List](3)(random[String]).apply())
    println(many2[Map](3)(random[String], random[Int]).apply())
    println(many2[Map](3).make[String, Int].apply())
    println(random[Either[String, Int]].apply())
    val a: Random[MyClass] = Random.oneOf[MyClass2, MyClass1].make
    println(many[List](10)(a).apply())
    val b: List[MyClass] = many[List](4000)(
      Random.oneOf[MyClass1, MyClass2, MyClass3, SubClass].make
    ).apply()
    println(b.count(_.isInstanceOf[MyClass1]))
    println(b.count(_.isInstanceOf[MyClass2]))
    println(b.count(_.isInstanceOf[MyClass3]))
    println(b.count(_.isInstanceOf[One]))
    println(many[List](10).make[Int].apply())

    println(uglyString(10)())
    println(string(10).apply())
    println(alphanumeric(10).apply())
    println(many[List](10)(oneOf(3, 4, 5, 7)).apply())
  }
}
