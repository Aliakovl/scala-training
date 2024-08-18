package dev.aliakovl.gin

import dev.aliakovl.gin.implicits._
import dev.aliakovl.gin.macros.RandomTransformer

object Main {
  def main(args: Array[String]): Unit = {

    val myClass: Random[MyClass2] = Random.random[MyClass2]

    val a = RandomTransformer.help[MyClass2, String](_.mc2field, myClass, Random.uglyString(100))

//    val a = random[MyClass2].help(_.string, Random.uglyString(151345))
    println(a)

//    println(ru.showRaw(ru.reify(random[MyClass2]).tree))
  }
}
