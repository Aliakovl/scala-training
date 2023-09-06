package dev.aliakovl.kernel.trans.reader

import dev.aliakovl.kernel.Monad
import dev.aliakovl.kernel.trans.MonadTrans

private object Inner:
  given[R]: MonadTrans[[M[_], A] =>> ReaderT[M, R, A]] with
    def lift[M[_] : Monad, A](ma: M[A]): ReaderT[M, R, A] = ReaderT { _ => ma }

  given[M[_] : Monad, R]: Monad[[A] =>> ReaderT[M, R, A]] with
    override def pure[A](a: A): ReaderT[M, R, A] = ReaderT { _ => Monad[M].pure(a) }

    extension[A] (readerT: ReaderT[M, R, A])
      def flatMap[B](f: A => ReaderT[M, R, B]): ReaderT[M, R, B] = ReaderT { r =>
        readerT.runReaderT(r).flatMap { a =>
          f(a).runReaderT(r)
        }
      }

  extension[M[_], R, A] (readerT: ReaderT[M, R, A])
    def mapReaderT[N[_], B](f: M[A] => N[B]): ReaderT[N, R, B] = ReaderT { r =>
      f(readerT.runReaderT(r))
    }

    def withReaderT[R1](f: R1 => R): ReaderT[M, R1, A] = ReaderT { r =>
      readerT.runReaderT(f(r))
    }

    def local(f: R => R): ReaderT[M, R, A] = withReaderT(f)

  def ask[M[_] : Monad, R]: ReaderT[M, R, R] = ReaderT { r => Monad[M].pure(r) }

  def asks[M[_] : Monad, R, A](f: R => A): ReaderT[M, R, A] = ReaderT { r =>
    Monad[M].pure(f(r))
  }

  def lift[M[_] : Monad, R, A](ma: M[A]): ReaderT[M, R, A] =
    MonadTrans[[MM[_], AA] =>> ReaderT[MM, R, AA]].lift[M, A](ma)

  def pure[M[_] : Monad, R, A](a: A): ReaderT[M, R, A] =
    Monad[[T] =>> ReaderT[M, R, T]].pure[A](a)