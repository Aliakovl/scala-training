package dev.aliakovl.gin

import Random._

sealed trait MyClass extends Product with Serializable
case class MyClass1(m: MyClass2) extends MyClass
case class MyClass2(int: Int, mc2field: String) extends MyClass
case class MyClass3() extends MyClass
case object MyClass4 extends MyClass

sealed trait SubClass extends MyClass
case class One() extends SubClass

object Main {
  def main(args: Array[String]): Unit = {
    println(random[Map[String, List[Option[MyClass]]]].get())
    println(many[List](3)(random[String]).get())
    println(many2[Map](3)(random[String], random[Int]).get())
    println(many2[Map](3).make[String, Int].get())
    println(random[Either[String, Int]].get())
    val a: Random[MyClass] = oneOf2[MyClass2, MyClass1].make
    println(many[List](10)(a).get())
    val b: List[MyClass] = many[List](4000)(
      oneOf4[MyClass1, MyClass2, MyClass3, SubClass].make
    ).get()
    println(b.count(_.isInstanceOf[MyClass1]))
    println(b.count(_.isInstanceOf[MyClass2]))
    println(b.count(_.isInstanceOf[MyClass3]))
    println(b.count(_.isInstanceOf[One]))
    println(many[List](10).make[Int].get())

    println(uglyString(10).get())
    println(string(10).get())
    println(alphanumeric(10).get())
    println(many[List](10)(oneOf(3, 4, 5, 7)).get())
  }
}
