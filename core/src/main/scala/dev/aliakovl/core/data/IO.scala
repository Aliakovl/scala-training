package dev.aliakovl.core.data

sealed trait IO[+A] { self =>
  def >>=[B](f: A => IO[B]): IO[B] =
    IO.FlatMap(self, f)

  def run(): A = self match {
    case IO.Pure(value)    => value
    case IO.FlatMap(fa, f) => f.asInstanceOf[Any => IO[A]](fa.run()).run()
  }
}

object IO {
  def pure[A](a: A): IO[A] = Pure(a)

  private final case class Pure[+A](value: A) extends IO[A]

  private final case class FlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IO[B]
}
