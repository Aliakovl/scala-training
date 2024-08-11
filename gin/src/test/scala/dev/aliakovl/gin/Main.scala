package dev.aliakovl.gin

sealed trait MyClass extends Product with Serializable
case class MyClass1(m: MyClass2) extends MyClass
case class MyClass2(int: Int, string: String) extends MyClass
case class MyClass3() extends MyClass
case object MyClass4 extends MyClass

object Main {
  def main(args: Array[String]): Unit = {
    println(Random[Map[String, List[Option[MyClass]]]].get())
    println(Random[String].get[List](3))
    println(Random[String, Int].get[Map](3))
    println(Random[Either[String, Int]].get())
    val a: Random[MyClass] = Random.oneOf2[MyClass2, MyClass1].make
    println(a.get[List](10))

    val b =
      Random.oneOf4[MyClass1, MyClass2, MyClass3, Int].make.get[List](4000)
    println(b.count(_.isInstanceOf[MyClass1]))
    println(b.count(_.isInstanceOf[MyClass2]))
    println(b.count(_.isInstanceOf[MyClass3]))
    println(b.count(_.isInstanceOf[Int]))
  }
}
