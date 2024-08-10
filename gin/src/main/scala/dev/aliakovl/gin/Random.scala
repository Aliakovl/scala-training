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
      ev: SubType[A, B]
  ): Random[ev.Out] = new Random[ev.Out] {
    override def get(): ev.Out = choose(ev.left(ar.get()), ev.right(br.get()))
  }

  private def choose[A](values: A*): A = {
    val i = ScalaRandom.nextInt(values.size)
    values(i)
  }
}

trait SubType[-A, -B] {
  type Out
  def left(a: A): Out
  def right(b: B): Out
}

object SubType {
  def make[A, B, T](left: A => T, right: B => T): SubType.Aux[A, B, T] = {
    val l = left
    val r = right
    new SubType[A, B] {
      override type Out = T
      override def left(a: A): T = l(a)
      override def right(b: B): T = r(b)
    }
  }

  type Aux[A, B, T] = SubType[A, B] { type Out = T }

  implicit def subType[A, B <: A]: SubType.Aux[A, B, A] =
    make[A, B, A](identity, identity)

  implicit def coproduct[A, B <: Coproduct]: SubType.Aux[A, B, A :+: B] =
    make[A, B, A :+: B](Inl.apply, Inr.apply)

  implicit def generic[A, B, C <: Coproduct, T](implicit
      gen: Generic.Aux[B, C],
      gen1: Generic.Aux[T, A :+: C],
      ev: SubType.Aux[A, C, A :+: C]
  ): SubType.Aux[A, B, T] =
    make(a => gen1.from(ev.left(a)), c => gen1.from(ev.right(gen.to(c))))

}
