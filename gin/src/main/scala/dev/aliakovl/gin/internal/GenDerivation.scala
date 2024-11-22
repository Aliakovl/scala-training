package dev.aliakovl.gin
package internal

import dev.aliakovl.gin.macros.GenMacros

import language.experimental.macros

trait GenDerivation {
  implicit def materialize[T]: Gen[T] = macro GenMacros.makeImpl[T]
}
