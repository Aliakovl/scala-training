package dev.aliakovl.core.data

import dev.aliakovl.core._

final case class Const[A, B](value: A)

object Const {
  implicit def constFunctor[C]: Functor[({type λ[α] = Const[C, α]})#λ] =
    new Functor[({type λ[α] = Const[C, α]})#λ] {
      override def map[A, B](f: A => B)(fa: Const[C, A]): Const[C, B] =
        Const(fa.value)
    }

  implicit def constApplicative[C: Monoid]: Applicative[({type λ[α] = Const[C, α]})#λ] =
    new Applicative[({type λ[α] = Const[C, α]})#λ] {
      override def pure[A](a: A): Const[C, A] = Const(implicitly[Monoid[C]].empty)

      override def ap[A, B](ff: Const[C, A => B])(fa: Const[C, A]): Const[C, B] =
        Const(fa.value)
    }
}
