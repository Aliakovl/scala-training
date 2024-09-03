package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import dev.aliakovl.gin.macros.GenMacro

import language.experimental.macros

trait RandomDerivation {
  implicit def materializeRandom[T]: Random[T] = macro GenMacro.materializeRandom[T]
}
