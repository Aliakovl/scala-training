package dev.aliakovl.gin

import dev.aliakovl.gin.macros.GenMacro

import scala.annotation.compileTimeOnly

final class Gen[A] private {
  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.Gen.specify")
  def specify[P, P1 <: P](selector: A => P)(random: Random[P1]): Gen[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.Gen.specify")
  def specifyConst[P, P1 <: P](selector: A => P)(random: P1): Gen[A] = ???

  def random: Random[A] = macro GenMacro.randomImpl[A]
}

object Gen {
  def apply[A]: Gen[A] = new Gen[A]
}
