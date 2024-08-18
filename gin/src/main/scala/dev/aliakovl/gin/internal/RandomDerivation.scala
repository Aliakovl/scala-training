package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import dev.aliakovl.gin.Random.random
import magnolia1._

import language.experimental.macros

trait RandomDerivation {
  type Typeclass[T] = Random[T]

  def join[T](ctx: CaseClass[Random, T]): Random[T] = random {
    ctx.construct(_.typeclass.get())
  }

  def split[T](ctx: SealedTrait[Random, T]): Random[T] = random {
    val index = scala.util.Random.nextInt(ctx.subtypes.size)
    ctx.subtypes(index).typeclass.get()
  }

  implicit def gen[T]: Random[T] = macro Magnolia.gen[T]
}