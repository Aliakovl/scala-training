package dev.aliakovl.kernel.trans.except

import dev.aliakovl.kernel.{Functor, Monad}
import dev.aliakovl.kernel.trans.MonadTrans

private object Inner:
  given[E]: MonadTrans[[M[_], A] =>> ExceptT[M, E, A]] with
    def lift[M[_] : Monad, A](ma: M[A]): ExceptT[M, E, A] = ExceptT(
      ma.flatMap { a =>
        Monad[M].pure[Either[E, A]](Right(a))
      }
    )

  given[M[_] : Monad, E]: Monad[[A] =>> ExceptT[M, E, A]] with
    def pure[A](a: A): ExceptT[M, E, A] = ExceptT(Monad[M].pure(Right(a)))

    extension[A] (ta: ExceptT[M, E, A])
      def flatMap[B](f: A => ExceptT[M, E, B]): ExceptT[M, E, B] = ExceptT(
        ta.runExceptT.flatMap {
          case Right(a) => f(a).runExceptT
          case Left(e) => Monad[M].pure(Left(e))
        }
      )

  extension[M[_], E, A] (ta: ExceptT[M, E, A])
    def mapExceptT[N[_], E1, B](f: M[Either[E, A]] => N[Either[E1, B]]): ExceptT[N, E1, B] = ExceptT(f(ta.runExceptT))

  extension[M[_] : Functor, E, A] (ta: ExceptT[M, E, A])
    def withExceptT[E1](f: E => E1): ExceptT[M, E1, A] = ExceptT(
      ta.runExceptT.map {
        case Left(e) => Left(f(e))
        case Right(a) => Right(a)
      }
    )

  extension[M[_] : Monad, E, A] (ta: ExceptT[M, E, A])
    def catchE[E1](f: E => ExceptT[M, E1, A]): ExceptT[M, E1, A] = ExceptT(
      ta.runExceptT.flatMap {
        case Left(e) => f(e).runExceptT
        case Right(a) => Monad[M].pure(Right(a))
      }
    )

    def fold[E1, B](failure: E => ExceptT[M, E1, B], success: A => ExceptT[M, E1, B]): ExceptT[M, E1, B] = ExceptT {
      ta.runExceptT.flatMap[Either[E1, B]] {
        case Left(e) => failure(e).runExceptT
        case Right(a) => success(a).runExceptT
      }
    }

    def tryE: ExceptT[M, E, Either[E, A]] = ExceptT(
      ta.runExceptT.map {
        case Left(e) => Right(Left(e))
        case Right(a) => Right(Right(a))
      }
    )

    def finallyE(closer: ExceptT[M, E, Unit]): ExceptT[M, E, A] =
      for {
        res <- ta.tryE
        _ <- closer
        a <- res.fold(throwE, pure)
      } yield a

  def handleE[M[_] : Monad, E, E1, A](f: E => ExceptT[M, E1, A])(t: ExceptT[M, E, A]): ExceptT[M, E1, A] =
    t.catchE(f)

  def throwE[M[_] : Monad, E, A](e: E): ExceptT[M, E, A] =
    ExceptT(Monad[M].pure(Left(e)))

  def lift[M[_] : Monad, E, A](ma: M[A]): ExceptT[M, E, A] =
    MonadTrans[[MM[_], AA] =>> ExceptT[MM, E, AA]].lift[M, A](ma)

  def pure[M[_] : Monad, E, A](a: A): ExceptT[M, E, A] =
    Monad[[T] =>> ExceptT[M, E, T]].pure[A](a)

  def right[M[_] : Monad, E, A](ma: M[A]): ExceptT[M, E, A] = lift(ma)

  def left[M[_] : Monad, E, A](me: M[E]): ExceptT[M, E, A] = ExceptT(
    me.flatMap { e =>
      Monad[M].pure[Either[E, A]](Left(e))
    }
  )
