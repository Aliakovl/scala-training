package dev.aliakovl.gin

import shapeless.{:+:, Coproduct, Generic, Inl, Inr}

trait Lub[-A, -B] {
  type Out
  def left(a: A): Out
  def right(b: B): Out
}

object Lub {
  def apply[A, B, T](implicit inst: Aux[A, B, T]): Aux[A, B, T] = inst

  type Aux[A, B, T] = Lub[A, B] { type Out = T }

  def make[A, B, T](left: A => T, right: B => T): Aux[A, B, T] = {
    val l = left
    val r = right
    new Lub[A, B] {
      override type Out = T
      override def left(a: A): T = l(a)
      override def right(b: B): T = r(b)
    }
  }

  implicit def subType[A, B <: A]: Aux[A, B, A] =
    make[A, B, A](identity, identity)

  implicit def coproduct[A, B <: Coproduct]: Aux[A, B, A :+: B] =
    make[A, B, A :+: B](Inl.apply, Inr.apply)

  implicit def generic[A, B, C <: Coproduct, T](implicit
      genB: Generic.Aux[B, C],
      genT: Generic.Aux[T, A :+: C],
      ev: Aux[A, C, A :+: C]
  ): Aux[A, B, T] =
    make(a => genT.from(ev.left(a)), b => genT.from(ev.right(genB.to(b))))
}
