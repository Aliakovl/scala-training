package dev.aliakovl.core

import dev.aliakovl.core.data.IO

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](f: A => B)(fa: F[A]): F[B] =
    flatMap(fa)(f andThen pure)

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(map(_)(fa))
}

object Monad {
  def apply[F[_]](implicit inst: Monad[F]): Monad[F] = inst

  implicit class MonadOps[F[_]: Monad, A](private val fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }

  implicit val monadIO: Monad[IO] = new Monad[IO] {
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      fa >>= f

    override def pure[A](a: A): IO[A] = IO.pure(a)
  }
}
