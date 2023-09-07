package dev.aliakovl.kernel

trait Functor[F[_]]:
  extension[A, FF[T] <: F[T]] (fa: FF[A])
    def map[B](f: A => B): F[B]
    def as[B](b: B): F[B] = fa.map(_ => b)

object Functor:
  def apply[F[_]](using f: Functor[F]): Functor[F] = f
