package dev.aliakovl.gin

import dev.aliakovl.gin.macros.GenMacro

import scala.annotation.compileTimeOnly

final class GenSpecify[A] private[gin] {
  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.specify")
  def specify[P](selector: A => P)(gen: Gen[P]): GenSpecify[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.specifyConst")
  def specifyConst[P](selector: A => P)(const: P): GenSpecify[A] = ???

  def make: Gen[A] = macro GenMacro.makeImpl[A]
}
