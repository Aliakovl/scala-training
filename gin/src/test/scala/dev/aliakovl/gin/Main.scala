package dev.aliakovl.gin

sealed trait MyClass
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
    val a: MyClass = Random.oneOf[MyClass2, MyClass1].get()
    println(a)
  }
}
