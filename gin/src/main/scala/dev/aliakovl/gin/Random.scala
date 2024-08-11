package dev.aliakovl.gin

import dev.aliakovl.gin.internal.GenericRandom
import shapeless.{:+:, Coproduct, Generic, Inl, Inr}

import scala.collection.Factory
import scala.language.implicitConversions
import scala.util.{Random => ScalaRandom}

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

  def oneOf[A, B](implicit
      ar: Random[A],
      br: Random[B],
      ev: Lub[A, B]
  ): Random[ev.Out] = {
    if (ScalaRandom.nextInt(2) == 0) {
      Random(ev.left(ar.get()))
    } else {
      Random(ev.right(br.get()))
    }
  }

  private def choose[A](values: A*): A = {
    val index = ScalaRandom.nextInt(values.size)
    values(index)
  }
}

