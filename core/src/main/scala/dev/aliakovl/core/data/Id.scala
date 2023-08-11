package dev.aliakovl.core.data

import dev.aliakovl.core._

object Id {
  def apply[A](a: A): Id[A] = a

  implicit val identityFunctor: Functor[Id] = new Functor[Id] {
    override def map[A, B](f: A => B)(fa: Id[A]): Id[B] = {
      f(fa)
    }
  }

  implicit val identityApplicative: Applicative[Id] = new Applicative[Id] {
    override def pure[A](a: A): Id[A] = a

    override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = {
      ff(fa)
    }
  }

}
