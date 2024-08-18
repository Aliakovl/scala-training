package dev.aliakovl.gin

import dev.aliakovl.gin.macros.RandomTransformer

import scala.language.experimental.macros

package object implicits {
  implicit class RandomTransformerOps[A](private val ra: Random[A])
      extends AnyVal {
    def specify[U](selector: A => U, ru: Random[U]): Random[A] =
      RandomTransformer.specify[A, U](selector, ra, ru)

//    def help[U](selector: A => U, ru: Random[U]): String =
//      RandomTransformer.help[A, U](selector, ra, ru)
  }
}
