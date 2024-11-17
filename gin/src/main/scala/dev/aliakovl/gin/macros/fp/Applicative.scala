package dev.aliakovl.gin.macros.fp

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map[A, B => (A, B)](fa)(a => b => (a, b)))(fb)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    ap(map(fa) { a: A => b: B =>
      f(a, b)
    })(fb)
  }
}

object Applicative {
  implicit val applicativeForOption: Applicative[Option] =
    new Applicative[Option] {
      override def pure[A](a: A): Option[A] = Some(a)

      override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
        ff.flatMap(fa.map)
    }

  implicit def applicativeForEither[E]
      : Applicative[({ type M[A] = Either[E, A] })#M] =
    new Applicative[({ type M[A] = Either[E, A] })#M] {
      override def pure[A](a: A): Either[E, A] = Right(a)

      override def ap[A, B](ff: Either[E, A => B])(
          fa: Either[E, A]
      ): Either[E, B] = ff.flatMap(fa.map)
    }
}
