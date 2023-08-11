package dev.aliakovl.free

import dev.aliakovl.core.Monad

final case class IO[A](unsafeRun: () => A)

object IO {
  def defer[A](a: => A): IO[A] = IO(() => a)

  implicit val monadIO: Monad[IO] = new Monad[IO] {
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO.defer {
      f(fa.unsafeRun()).unsafeRun()
    }

    override def pure[A](a: A): IO[A] = IO.defer(a)
  }
}
