package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Gen
import dev.aliakovl.gin.internal.GenMany._

import scala.collection.Factory

trait GenMany {
  def many[C[E] <: IterableOnce[E]](size: Int): ApplyMany[C] =
    new ApplyMany[C](size)

  def many2[M[K, V] <: Iterable[(K, V)]](size: Int): ApplyMany2[M] =
    new ApplyMany2[M](size)
}

object GenMany {
  final class ApplyMany[C[E] <: IterableOnce[E]](private val size: Int)
      extends AnyVal {
    def apply[A](ra: Gen[A])(implicit
        f: Factory[A, C[A]]
    ): Gen[C[A]] = Gen {
      f.fromSpecific(Iterable.fill(size)(ra.apply()))
    }

    def make[A](implicit
        ra: Gen[A],
        f: Factory[A, C[A]]
    ): Gen[C[A]] = apply(ra)
  }

  final class ApplyMany2[M[K, V] <: Iterable[(K, V)]](private val size: Int)
      extends AnyVal {
    def apply[K, V](rk: Gen[K], rv: Gen[V])(implicit
        f: Factory[(K, V), M[K, V]]
    ): Gen[M[K, V]] = Gen {
      f.fromSpecific(
        Iterable
          .fill(size)(rk.apply())
          .zip(Iterable.fill(size)(rv.apply()))
      )
    }

    def make[K, V](implicit
        rk: Gen[K],
        rv: Gen[V],
        f: Factory[(K, V), M[K, V]]
    ): Gen[M[K, V]] = apply(rk, rv)
  }
}
