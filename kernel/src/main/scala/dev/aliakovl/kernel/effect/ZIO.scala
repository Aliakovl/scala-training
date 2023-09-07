package dev.aliakovl.kernel.effect

import dev.aliakovl.kernel.trans.except.ExceptT
import dev.aliakovl.kernel.trans.reader.ReaderT

type ZIO[-R, +E, +A] = ReaderT[[T] =>> ExceptT[IO, E, T], R, A]

object ZIO:
  def fromReader[R, E, A](r: R => ExceptT[IO, E, A]): ZIO[R, E, A] = ReaderT(r)

  def success[A](a: => A): ZIO[Any, Nothing, A] = ReaderT.lift(ExceptT.lift(IO.success(a)))

  def fail[E](e: => E): ZIO[Any, E, Nothing] = ReaderT.lift(ExceptT.throwE(e))

  def service[R]: ZIO[R, Nothing, R] = ReaderT.ask[[T] =>> ExceptT[IO, Nothing, T], R]

  def serviceWith[R]: ServiceWithPartiallyApplied[R] = ServiceWithPartiallyApplied[R]()

  extension[R, E, A] (zio: ZIO[R, E, A])
    def foldZIO[E2, B](failure: E => ZIO[R, E2, B], success: A => ZIO[R, E2, B]): ZIO[R, E2, B] = fromReader[R, E2, B] { r =>
      zio.runReaderT(r).fold(
        e => failure(e).runReaderT(r),
        a => success(a).runReaderT(r)
      )
    }

    def catchAll[E2](h: E => ZIO[R, E2, A]): ZIO[R, E2, A] = foldZIO(h, success)

    def mapError[E2](f: E => E2): ZIO[R, E2, A] = fromReader { r =>
      zio.runReaderT(r).withExceptT(f)
    }

    def provide(environment: R): ZIO[Any, E, A] = fromReader { _ =>
      zio.runReaderT(environment)
    }

  extension[E, A] (zio: ZIO[Any, E, A])
    def runZIO: Either[E, A] = zio.runReaderT(()).runExceptT.runUnsafe

  final class ServiceWithPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: R => A): ZIO[R, Nothing, A] = ReaderT.asks(f)
  }
