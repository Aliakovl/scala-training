package dev.aliakovl.kernel.effect

import dev.aliakovl.kernel.trans.except.ExceptT
import dev.aliakovl.kernel.trans.reader.ReaderT
import dev.aliakovl.kernel.Console

import java.io.IOException

type ZIO[-R, +E, +A] = ReaderT[[T] =>> ExceptT[IO, E, T], R, A]

object ZIO:
  def fromReader[R, E, A](r: R => ExceptT[IO, E, A]): ZIO[R, E, A] = ReaderT(r)

  def attempt[A](code: => A): ZIO[Any, Throwable, A] = ZIO.suspendSucceed(
    try {
      val result = code
      ZIO.success(result)
    } catch {
      case t: Throwable => ZIO.fail(t)
    }
  )

  def success[A](a: => A): ZIO[Any, Nothing, A] =
    ReaderT.lift(ExceptT.lift(IO.success(a)))

  def suspendSucceed[R, E, A](zio: => ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.success(zio).flatMap(x => x)

  def fail[E](e: => E): ZIO[Any, E, Nothing] = ReaderT.lift(ExceptT.throwE(e))

  def service[R]: ZIO[R, Nothing, R] =
    ReaderT.ask[[T] =>> ExceptT[IO, Nothing, T], R]

  def serviceWith[R]: [A] => (R => A) => ZIO[R, Nothing, A] = [A] =>
    (f: R => A) => ReaderT.asks(f)

  def printLine(line: => Any): ZIO[Any, IOException, Unit] =
    Console[[T] =>> ZIO[Any, IOException, T]].printLine(line)

  extension [R, E, A](zio: ZIO[R, E, A])
    def foldZIO[R1 <: R, E1, B](
        failure: E => ZIO[R1, E1, B],
        success: A => ZIO[R1, E1, B]
    ): ZIO[R1, E1, B] = fromReader[R1, E1, B] { r =>
      zio
        .runReaderT(r)
        .fold(
          e => failure(e).runReaderT(r),
          a => success(a).runReaderT(r)
        )
    }

    def catchAll[R1 <: R, E1, A1 >: A](
        h: E => ZIO[R1, E1, A1]
    ): ZIO[R1, E1, A1] = foldZIO(h, success)

    def mapError[E2](f: E => E2): ZIO[R, E2, A] = fromReader { r =>
      zio.runReaderT(r).withExceptT(f)
    }

    def refineOrDieWith[E1](
        pf: PartialFunction[E, E1]
    )(f: E => Throwable): ZIO[R, E1, A] = {
      zio.foldZIO(
        e => {
          if pf.isDefinedAt(e) then ZIO.fail(pf.apply(e))
          else throw f(e)
        },
        a => ZIO.success(a)
      )
    }

    def refineOrDie[E1](
        pf: PartialFunction[E, E1]
    )(implicit ev: E <:< Throwable): ZIO[R, E1, A] = {
      refineOrDieWith(pf)(ev)
    }

    def provide(environment: R): ZIO[Any, E, A] = fromReader { _ =>
      zio.runReaderT(environment)
    }

    def exit: ZIO[R, Nothing, Either[E, A]] =
      fromReader[R, Nothing, Either[E, A]] { r =>
        zio.runReaderT(r).tryT
      }

  extension [R, E <: Throwable, A](zio: ZIO[R, E, A])
    def refineToOrDie[E1 <: E]: ZIO[R, E1, A] = zio.refineOrDie { case e: E1 =>
      e
    }

    def orDie: ZIO[R, Nothing, A] = zio.catchAll { e => throw e }

  extension [E <: Throwable, A](zio: ZIO[Any, E, A])
    def run(): Unit =
      new IORuntime(zio.orDie.runReaderT(()).runExceptT).runUnsafe(_ => ())
