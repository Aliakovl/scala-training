package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Gen
import dev.aliakovl.gin.macros.GenMacro

import language.experimental.macros

trait GenDerivation {
  implicit def materialize[T]: Gen[T] = macro GenMacro.materializeImpl[T]
}
