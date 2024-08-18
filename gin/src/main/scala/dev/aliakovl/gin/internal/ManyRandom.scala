package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import dev.aliakovl.gin.Random.random
import dev.aliakovl.gin.internal.ManyRandom._

import scala.collection.Factory

trait ManyRandom {
  def many[C[E] <: IterableOnce[E]](size: Int): ApplyMany[C] =
    new ApplyMany[C](size)

  def many2[M[K, V] <: Iterable[(K, V)]](size: Int): ApplyMany2[M] =
    new ApplyMany2[M](size)
}

object ManyRandom {
  final class ApplyMany[C[E] <: IterableOnce[E]](private val size: Int)
      extends AnyVal {
    def apply[A](ra: Random[A])(implicit
        f: Factory[A, C[A]]
    ): Random[C[A]] = random {
      f.fromSpecific(Iterable.fill(size)(ra.get()))
    }

    def make[A](implicit
        ra: Random[A],
        f: Factory[A, C[A]]
    ): Random[C[A]] = apply(ra)
  }

  final class ApplyManyArray[C[E] <: Array[E]](private val size: Int)
      extends AnyVal {
    def apply[A](ra: Random[A])(implicit
        f: Factory[A, C[A]]
    ): Random[C[A]] = random {
      f.fromSpecific(Iterable.fill(size)(ra.get()))
    }

    def make[A](implicit
        ra: Random[A],
        f: Factory[A, C[A]]
    ): Random[C[A]] = apply(ra)
  }

  final class ApplyMany2[M[K, V] <: Iterable[(K, V)]](private val size: Int)
      extends AnyVal {
    def apply[K, V](rk: Random[K], rv: Random[V])(implicit
        f: Factory[(K, V), M[K, V]]
    ): Random[M[K, V]] = random {
      f.fromSpecific(
        Iterable
          .fill(size)(rk.get())
          .zip(Iterable.fill(size)(rv.get()))
      )
    }

    def make[K, V](implicit
        rk: Random[K],
        rv: Random[V],
        f: Factory[(K, V), M[K, V]]
    ): Random[M[K, V]] = apply(rk, rv)
  }
}
