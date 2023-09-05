package dev.aliakovl.kernel.data

import dev.aliakovl.kernel.Monad

type Id[A] = A

object Id:
  def apply[A](a: A): Id[A] = a

  given Monad[Id] with
    override def pure[A](a: A): Id[A] = a

    extension[A] (ma: Id[A])
      def flatMap[B](f: A => Id[B]): Id[B] = f(ma)
