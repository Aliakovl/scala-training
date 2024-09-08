package dev.aliakovl.gin

import dev.aliakovl.gin.macros.GenMacro

import scala.annotation.compileTimeOnly

final class Gen[A] private {
  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.Gen.specify")
  def specify[P](selector: A => P)(random: Random[P]): Gen[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.Gen.specifyConst")
  def specifyConst[P](selector: A => P)(random: P): Gen[A] = ???

  def random: Random[A] = macro GenMacro.randomImpl[A]
}

object Gen {
  def apply[A]: Gen[A] = new Gen[A]
}
