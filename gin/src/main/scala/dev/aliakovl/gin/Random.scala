package dev.aliakovl.gin

import dev.aliakovl.gin.internal.GenericRandom

import scala.collection.Factory
import scala.language.implicitConversions

trait Random[A] { self =>
  def get(): A
  def get[C[E] <: IterableOnce[E]](size: Int)(implicit
      f: Factory[A, C[A]],
      ev: Random[A]
  ): C[A] = Random.iterableGenerator[A, C](size).get()
}

object Random extends GeneratorInstances with GenericRandom {
  def apply[A](implicit g: Random[A]): Random[A] = g
}
