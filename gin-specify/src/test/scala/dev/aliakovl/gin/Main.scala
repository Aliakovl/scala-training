package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{const, random, uglyString}
import dev.aliakovl.gin.{MyClass1, MyClass2}
import dev.aliakovl.gin.macros.{GenImpl, GenOps, RandomTransformer}

object Main {
  def main(args: Array[String]): Unit = {

//    val myClass: Random[MyClass2] = Random.random[MyClass2]

//    val a = RandomTransformer.help[MyClass2, String](
//      _.mc2field,
//      random[MyClass2],
//      uglyString(100)
//    )
//
//    println(a)
//
//    val wqef = RandomTransformer.specify[MyClass2, String](
//      _.mc2field,
//      random[MyClass2],
//      uglyString(5)
//    )
//    println(wqef.get())

//    println(ru.showRaw(ru.reify(random[MyClass2]).tree))

    val r: String =
      Gen[MyClass]
        .specify[String](_.when[MyClass1].m.mc2field, uglyString(100))
        .specify[Int](_.when[MyClass2].int, const(4))
        .random

    println(r)

  }
}
