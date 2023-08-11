package dev.aliakovl.free

import dev.aliakovl.core.{Monad, ~>}

sealed trait Free[M[_], A] { self =>

  import Free._

  def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))

  def foldMap[G[_]: Monad](implicit natTrans: M ~> G): G[A] = self match {
    case Pure(a)     => Monad[G].pure(a)
    case Suspend(ma) => natTrans.apply(ma)
    case FlatMap(fa, f) =>
      Monad[G].flatMap(fa.foldMap)(a =>
        f.asInstanceOf[Any => Free[M, A]](a).foldMap
      )
  }
}

object Free {
  def pure[M[_], A](a: A): Free[M, A] = Pure(a)

  def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)

  final private case class Pure[M[_], A](a: A) extends Free[M, A]
  final private case class FlatMap[M[_], A, B](
      fa: Free[M, A],
      f: A => Free[M, B]
  ) extends Free[M, B]
  final private case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]
}
