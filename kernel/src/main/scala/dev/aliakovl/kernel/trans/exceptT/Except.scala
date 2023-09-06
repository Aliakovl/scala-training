package dev.aliakovl.kernel.trans.exceptT

import dev.aliakovl.kernel.Monad
import dev.aliakovl.kernel.data.Id

type Except[E, A] = ExceptT[Id, E, A]

object Except:
  def except[M[_] : Monad, E, A](either: Either[E, A]): ExceptT[M, E, A] =
    ExceptT(Monad[M].pure(either))

  extension[E, A] (except: Except[E, A])
    def runExcept: Either[E, A] = except.run
    def mapExcept[E1, B](f: Either[E, A] => Either[E1, B]): Except[E1, B] =
      ExceptT(f(except.run))
    def withExcept[E1](f: E => E1): Except[E1, A] = ExceptT[Id, E1, A](
      except.run match
        case Left(e) => Left(f(e))
        case Right(a) => Right(a)
    )
