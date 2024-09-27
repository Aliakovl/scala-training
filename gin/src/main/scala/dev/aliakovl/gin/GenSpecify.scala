package dev.aliakovl.gin

import dev.aliakovl.gin.macros.GenMacro

import scala.annotation.compileTimeOnly

final class GenSpecify[A] private[gin] {
  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.specify, try call dev.aliakovl.gin.GenSpecify.make at the end")
  def specify[P](selector: A => P)(gen: Gen[P]): GenSpecify[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.specifyConst, try call dev.aliakovl.gin.GenSpecify.make at the end")
  def specifyConst[P](selector: A => P)(const: P): GenSpecify[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.useDefault, try call dev.aliakovl.gin.GenSpecify.make at the end")
  def useDefault[P](selector: A => P): GenSpecify[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.exclude, try call dev.aliakovl.gin.GenSpecify.make at the end")
  def exclude[B <: A]: GenSpecify[A] = ???

  def make: Gen[A] = macro GenMacro.makeImpl[A]
}
