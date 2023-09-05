package dev.aliakovl.core.data

import dev.aliakovl.core._

object Id {
  def apply[A](a: A): Id[A] = a

  implicit val identityMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def pure[A](a: A): Id[A] = a
  }
}
