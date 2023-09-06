package dev.aliakovl.kernel.trans.except

import dev.aliakovl.kernel.Monad
import dev.aliakovl.kernel.data.Id


type Except[E, A] = ExceptT[Id, E, A]

object Except:
  export Inner.given
  export Inner.*

  def except[M[_] : Monad, E, A](either: Either[E, A]): ExceptT[M, E, A] =
    ExceptT(Monad[M].pure(either))

  extension[E, A] (except: Except[E, A])
    def runExcept: Either[E, A] = except.runExceptT.runId
    def mapExcept[E1, B](f: Either[E, A] => Either[E1, B]): Except[E1, B] =
      ExceptT(Id(f(except.runExceptT.runId)))
    def withExcept[E1](f: E => E1): Except[E1, A] = ExceptT[Id, E1, A](
      except.runExceptT.runId match
        case Left(e) => Id(Left(f(e)))
        case Right(a) => Id(Right(a))
    )
