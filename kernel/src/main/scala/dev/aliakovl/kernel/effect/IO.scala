package dev.aliakovl.kernel.effect

import dev.aliakovl.kernel.Monad

sealed trait IO[+A] {
  self =>
  def runUnsafe: A = self match
    case IO.Success(value) => value()
    case IO.FlatMap(fa, f) => f(fa.runUnsafe).runUnsafe
}

object IO:
  def success[A](value: => A): IO[A] = Success(() => value)

  private final case class Success[A](value: () => A) extends IO[A]

  private final case class FlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IO[B]

  given Monad[IO] with
    def pure[A](a: A): IO[A] = Success(() => a)

    extension[A, MM[T] <: IO[T]] (ma: MM[A])
      def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(ma, f)
