package dev.aliakovl.kernel.effect

import dev.aliakovl.kernel.trans.except.ExceptT
import dev.aliakovl.kernel.trans.reader.ReaderT

type ZIO[R, E, A] = ReaderT[[T] =>> ExceptT[IO, E, T], R, A]

object ZIO:

  def success[A](a: => A): ZIO[Any, Nothing, A] = ReaderT.lift(ExceptT.lift(IO.success(a)))

  def service[R]: ZIO[R, Nothing, R] = ReaderT.ask[[T] =>> ExceptT[IO, Nothing, T], R]

  def serviceWith[R]: ServiceWithPartiallyApplied[R] = ServiceWithPartiallyApplied[R]()

  extension[R, E, A] (zio: ZIO[R, E, A])
    def provide(environment: R): ZIO[Any, E, A] = ReaderT { _ =>
      zio.runReaderT(environment)
    }

  final class ServiceWithPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: R => A): ZIO[R, Nothing, A] = ReaderT.asks(f)
  }
