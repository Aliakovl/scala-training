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
    val a: MyClass = Random.oneOf[MyClass2, MyClass1].get()
    println(a)

    val b: List[MyClass] = List.fill(3000)(Random.oneOf[MyClass1, MyClass2, MyClass3].get())
    println(b.count(_.isInstanceOf[MyClass1]))
    println(b.count(_.isInstanceOf[MyClass2]))
    println(b.count(_.isInstanceOf[MyClass3]))

    val f1: List[MyClass] = Union.f(MyClass3(), MyClass4)
    val f2: List[MyClass] = Union[MyClass3, MyClass4.type].f(MyClass3(), MyClass4)
  }
}


object Union {
  def apply[A, B]: Union[A, B] = new Union[A, B] {}

  def f[T, A <: T, B <: T](a: A, b: B): List[T] = List[T](a,b)
}

trait Union[A, B] {
  def f[T, A1 >: A <: T, B1 >: B <: T](a: A1, b: B1): List[T] = List[T](a, b)
}
