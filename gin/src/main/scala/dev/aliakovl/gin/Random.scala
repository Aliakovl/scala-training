package dev.aliakovl.gin

import dev.aliakovl.gin.internal.GenericRandom

import scala.collection.Factory
import scala.language.implicitConversions
import scala.util.{Random => ScalaRandom}

trait Random[A] { self =>
  def get(): A

  def map[B](f: A => B): Random[B] = Random(f(get()))

  def widen[T >: A]: Random[T] = Random(get())

  def get[C[E] <: IterableOnce[E]](size: Int)(implicit
      f: Factory[A, C[A]]
  ): C[A] = f.fromSpecific(Iterable.fill(size)(get()))
}

object Random extends GeneratorInstances with GenericRandom {
  def apply[A](const: => A): Random[A] = () => const

  def apply[A](implicit g: Random[A]): Random[A] = g

  def apply[K, V]: ApplyGet2d[K, V] = new ApplyGet2d[K, V]

  def oneOf2[A, B]: ApplyOneOf2[A, B] = new ApplyOneOf2[A, B]()

  def oneOf3[A, B, C]: ApplyOneOf3[A, B, C] = new ApplyOneOf3[A, B, C]()

  private def choose[A](values: A*): A = {
    val index = ScalaRandom.nextInt(values.size)
    values(index)
  }

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

  final class ApplyOneOf2[A, B](private val dummy: Boolean = true) {
    def make[T, A1 >: A <: T, B1 >: B <: T](implicit
        ra: Random[A1],
        rb: Random[B1]
    ): Random[T] = choose(ra.widen[T], rb.widen[T])
  }

  final class ApplyOneOf3[A, B, C](private val dummy: Boolean = true) {
    def make[T, A1 >: A <: T, B1 >: B <: T, C1 >: C <: T](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1]
    ): Random[T] = choose(ra.widen[T], rb.widen[T], rc.widen[T])
  }
}
