package dev.aliakovl.core

trait Functor[F[_]] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]
}

object Functor {
  def apply[F[_]](implicit inst: Functor[F]): Functor[F] = inst

  implicit class FunctorOps[F[_]: Functor, A](private val fa: F[A]) {
    def map[B](f: A => B): F[B] = Functor[F].map(f)(fa)
    def as[B](b: => B): F[B] = Functor[F].map[A, B](_ => b)(fa)
  }
}
