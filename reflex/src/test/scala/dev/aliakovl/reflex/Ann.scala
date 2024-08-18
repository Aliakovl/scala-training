package dev.aliakovl.reflex

import scala.reflect.runtime.universe._

object Ann {
  def paramInfo[T](x: T)(implicit tag: TypeTag[T]): Unit = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    println(s"type of $x has type arguments $targs")
  }

  def weakParamInfo[T](x: T)(implicit tag: WeakTypeTag[T]): Unit = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    println(s"type of $x has type arguments $targs")
  }

  def main(args: Array[String]): Unit = {

    val listTpe = typeTag[List[Int]].tpe

    val finals = listTpe.members.sorted.filter(_.isFinal)

    println(finals)

    Constant(true) match {
      case Constant(s: String)  => println("A string: " + s)
      case Constant(b: Boolean) => println("A Boolean value: " + b)
      case Constant(x)          => println("Something else: " + x)
    }
    assert(Constant(true).value == true)

    paramInfo(paramInfo[Int] _)

    def foo[T] = {
      weakParamInfo(List[T]())
    }

    foo[Int]

    import Macros._

    printf("hello %s! wef %d", "world", 2)

  }
}
