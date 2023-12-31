package dev.aliakovl.kernel

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]

  extension [A, B, FF[T] <: F[T]](ff: FF[A => B]) def ap(fa: F[A]): F[B]

  extension [A, FF[T] <: F[T]](fa: FF[A])
    override def map[B](f: A => B): F[B] = ap(pure(f))(fa)

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.map[B => (A, B)](a => b => (a, b)).ap(fb)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    zip(fa, fb).map(f.tupled)

  def zip3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    fa.map[B => C => (A, B, C)](a => b => c => (a, b, c)).ap(fb).ap(fc)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    zip3(fa, fb, fc).map(f.tupled)

object Applicative:
  def pure[F[_]]: [A] => A => Applicative[F] ?=> F[A] =
    [A] => (a: A) => (inst: Applicative[F]) ?=> inst.pure(a)
