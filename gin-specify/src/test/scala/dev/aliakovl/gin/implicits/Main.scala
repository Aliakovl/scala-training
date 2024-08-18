package dev.aliakovl.gin.implicits

import dev.aliakovl.gin.Random.{random, uglyString}
import dev.aliakovl.gin.{MyClass2, Random}
import dev.aliakovl.gin.macros.RandomTransformer

object Main {
  def main(args: Array[String]): Unit = {

//    val myClass: Random[MyClass2] = Random.random[MyClass2]

    val a = RandomTransformer.help[MyClass2, String](
      _.mc2field,
      Random.random[MyClass2],
      Random.uglyString(100)
    )

    println(a)

    val wqef = RandomTransformer.specify[MyClass2, String](_.mc2field, random[MyClass2], uglyString(5))
    println(wqef.get())

//    println(ru.showRaw(ru.reify(random[MyClass2]).tree))
  }
}
