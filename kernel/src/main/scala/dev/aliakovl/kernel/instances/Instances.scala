package dev.aliakovl.kernel.instances

import dev.aliakovl.kernel.Monad

object Instances:
  given Monad[Option] with
    def pure[A](a: A): Option[A] = Some(a)

    extension[A, MM[T] <: Option[T]] (ma: MM[A])
      def flatMap[B](f: A => Option[B]): Option[B] = ma match
        case Some(x) => f(x)
        case None => None

  given[E]: Monad[[T] =>> Either[E, T]] with
    def pure[A](a: A): Either[E, A] = Right(a)

    extension[A, MM[T] <: Either[E, T]] (ma: MM[A])
      def flatMap[B](f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
