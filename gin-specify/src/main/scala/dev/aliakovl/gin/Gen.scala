package dev.aliakovl.gin

import dev.aliakovl.gin.macros.{GenMacro, GenOps}

import scala.annotation.compileTimeOnly

final class Gen[A] private {
  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.Gen.specify")
  def specify[P](selector: A => P, random: Random[P]): Gen[A] = ???
}

object Gen {
  import scala.language.experimental.macros
  import scala.language.implicitConversions

  def apply[A]: Gen[A] = new Gen[A]

  implicit def genOps[A](gen: Gen[A]): GenOps[A] = macro GenMacro.randomImpl[A]
}
