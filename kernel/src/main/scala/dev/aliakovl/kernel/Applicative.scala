package dev.aliakovl.kernel

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]

  extension[A, B] (ff: F[A => B])
    def ap(fa: F[A]): F[B]

  extension[A] (fa: F[A])
    override def map[B](f: A => B): F[B] = ap(pure(f))(fa)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.map[B => (A, B)](a => b => (a, b)).ap(fb)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    product(fa, fb).map(f.tupled)

  def product3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    fa.map[B => C => (A, B, C)](a => b => c => (a, b, c)).ap(fb).ap(fc)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    product3(fa, fb, fc).map(f.tupled)

object Applicative:
  def apply[F[_]](using f: Applicative[F]): Applicative[F] = f
