package dev.aliakovl.gin

import dev.aliakovl.gin.internal.GenericRandom

import scala.collection.Factory
import scala.language.implicitConversions

trait Random[A] { self =>
  def get(): A

  def get[C[E] <: IterableOnce[E]](size: Int)(implicit
      f: Factory[A, C[A]],
      ra: Random[A]
  ): C[A] = f.fromSpecific(Iterable.fill(size)(Random[A].get()))
}

object Random extends GeneratorInstances with GenericRandom {
  def apply[A](const: => A): Random[A] = () => const

  def apply[A](implicit g: Random[A]): Random[A] = g

  def apply[K, V]: ApplyGet2d[K, V] = new ApplyGet2d[K, V]

  final class ApplyGet2d[K, V](private val dummy: Boolean = true)
      extends AnyVal {
    def get[M[Kk, VV] <: Iterable[(Kk, VV)]](size: Int)(implicit
        f: Factory[(K, V), M[K, V]],
        rk: Random[K],
        rv: Random[V]
    ): M[K, V] = f.fromSpecific(
      Iterable
        .fill(size)(Random[K].get())
        .zip(Iterable.fill(size)(Random[V].get()))
    )
  }
}
