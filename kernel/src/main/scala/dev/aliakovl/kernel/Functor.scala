package dev.aliakovl.kernel

trait Functor[F[_]]:
  extension[A] (fa: F[A]) def map[B](f: A => B): F[B]

object Functor {
  def apply[F[_]](using f: Functor[F]): Functor[F] = f
}
