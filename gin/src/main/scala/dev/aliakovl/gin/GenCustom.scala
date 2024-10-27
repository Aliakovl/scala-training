package dev.aliakovl.gin

import dev.aliakovl.gin.macros.GenMacro

import scala.annotation.compileTimeOnly

final class GenCustom[A] private[gin] {
  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.specify, try to call .make at the end")
  def specify[P](selector: A => P)(gen: Gen[P]): GenCustom[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.specifyConst, try to call .make at the end")
  def specifyConst[P](selector: A => P)(const: P): GenCustom[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.useDefault, try to call .make at the end")
  def useDefault[P](selector: A => P): GenCustom[A] = ???

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.GenSpecify.exclude, try to call .make at the end")
  def exclude[P](selector: A => P): GenCustom[A] = ???

  def make: Gen[A] = macro GenMacro.materializeImpl[A]
}
