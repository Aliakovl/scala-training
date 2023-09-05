package dev.aliakovl.kernel.trans.data

import dev.aliakovl.kernel.{Functor, Monad}
import dev.aliakovl.kernel.trans.MonadTrans

final case class ExceptT[M[_], E, A](run: M[Either[E, A]]) {
  def mapExceptT[N[_], E1, B](f: M[Either[E, A]] => N[Either[E1, B]]): ExceptT[N, E1, B] = ExceptT(f(run))
}

object ExceptT:
  given[E]: MonadTrans[[M[_], A] =>> ExceptT[M, E, A]] with
    def lift[M[_] : Monad, A](ma: M[A]): ExceptT[M, E, A] =
      ExceptT(
        ma.flatMap { a =>
          Monad[M].pure[Either[E, A]](Right(a))
        }
      )

  given[M[_] : Monad, E]: Monad[[A] =>> ExceptT[M, E, A]] with
    def pure[A](a: A): ExceptT[M, E, A] = ExceptT(Monad[M].pure(Right(a)))

    extension[A] (tma: ExceptT[M, E, A]) def flatMap[B](f: A => ExceptT[M, E, B]): ExceptT[M, E, B] =
      ExceptT(
        tma.run.flatMap {
          case Right(a) => f(a).run
          case Left(e) => Monad[M].pure(Left(e))
        }
      )

  extension[M[_] : Functor, E, A] (t: ExceptT[M, E, A])
    def withExceptT[E1](f: E => E1): ExceptT[M, E1, A] = ExceptT(
      t.run.map {
        case Left(e) => Left(f(e))
        case Right(a) => Right(a)
      }
    )

  def throwE[M[_] : Monad, E, A](e: E): ExceptT[M, E, A] = ExceptT(Monad[M].pure(Left(e)))

  def lift[M[_] : Monad, E, A](ma: M[A]): ExceptT[M, E, A] = MonadTrans[[MM[_], AA] =>> ExceptT[MM, E, AA]].lift[M, A](ma)

  def right[M[_] : Monad, E, A](ma: M[A]): ExceptT[M, E, A] = lift(ma)

  def left[M[_] : Monad, E, A](me: M[E]): ExceptT[M, E, A] = ExceptT(
    me.flatMap { e =>
      Monad[M].pure[Either[E, A]](Left(e))
    }
  )
