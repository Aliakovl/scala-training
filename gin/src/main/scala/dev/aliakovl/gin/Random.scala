package dev.aliakovl.gin

import dev.aliakovl.gin.internal.GenericRandom

import scala.collection.Factory
import scala.language.implicitConversions
import scala.util.{Random => ScalaRandom}

trait Random[A] { self =>
  def get(): A

  def map[B](f: A => B): Random[B] = Random(f(get()))

  def get[C[E] <: IterableOnce[E]](size: Int)(implicit
      f: Factory[A, C[A]]
  ): C[A] = f.fromSpecific(Iterable.fill(size)(get()))
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

  def oneOf[A, B, C](implicit
      ar: Random[A],
      br: Random[B],
      cr: Random[C],
      ev: Lub3[A, B, C]
  ): Random[ev.Out] = {
    ScalaRandom.nextInt(3) match {
      case 0 => ar.map(ev.f1)
      case 1 => br.map(ev.f2)
      case 2 => cr.map(ev.f3)
    }
  }

  private def choose[A](values: A*): A = {
    val index = ScalaRandom.nextInt(values.size)
    values(index)
  }
}
