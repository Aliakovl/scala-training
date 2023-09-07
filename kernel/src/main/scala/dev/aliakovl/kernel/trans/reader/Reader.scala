package dev.aliakovl.kernel.trans.reader

import dev.aliakovl.kernel.Monad
import dev.aliakovl.kernel.data.Id
import dev.aliakovl.kernel.trans.reader.Inner.{mapReaderT, withReaderT}

type Reader[-R, +A] = ReaderT[Id, R, A]

object Reader:
  export Inner.given
  export Inner.*

  def reader[M[+_] : Monad, R, A](f: R => A): ReaderT[M, R, A] = ReaderT[M, R, A] { r =>
    summon[Monad[M]].pure(f(r))
  }

  extension[R, A] (reader: Reader[R, A])
    def mapReader[B](f: A => B): Reader[R, B] = reader.mapReaderT { a =>
      Id(f(a.runId))
    }
    def withReader[R1](f: R1 => R): Reader[R1, A] = reader.withReaderT(f)
