package dev.aliakovl.gin

import shapeless.{:+:, Coproduct, Generic, Inl, Inr}

trait Lub3[-A, -B, -C] {
  type Out
  def f1(a: A): Out
  def f2(b: B): Out
  def f3(c: C): Out
}

object Lub3 {
  def apply[A, B, C, T](implicit inst: Aux[A, B, C, T]): Aux[A, B, C, T] = inst
  type Aux[A, B, C, T] = Lub3[A, B, C] { type Out = T }

  def make[A, B, C, T](
      _f1: A => T,
      _f2: B => T,
      _f3: C => T
  ): Aux[A, B, C, T] = {
    new Lub3[A, B, C] {
      override type Out = T
      override def f1(a: A): T = _f1(a)
      override def f2(b: B): T = _f2(b)
      override def f3(c: C): T = _f3(c)
    }
  }

  implicit def subType[A, B <: A, C <: B]: Aux[A, B, C, A] =
    make[A, B, C, A](identity, identity, identity)

  implicit def coproduct[A, B, C <: Coproduct]: Aux[A, B, C, A :+: B :+: C] =
    make[A, B, C, A :+: B :+: C](
      a => Inl(a),
      b => Inr(Inl(b)),
      c => Inr(Inr(c))
    )

  implicit def generic[A, B, C, D <: Coproduct, T](implicit
      genB: Generic.Aux[C, D],
      genT: Generic.Aux[T, A :+: B :+: D],
      ev: Aux[A, B, D, A :+: B :+: D]
  ): Aux[A, B, C, T] =
    make(
      a => genT.from(ev.f1(a)),
      b => genT.from(ev.f2(b)),
      c => genT.from(ev.f3(genB.to(c)))
    )
}
