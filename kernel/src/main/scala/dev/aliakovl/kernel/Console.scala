package dev.aliakovl.kernel

import dev.aliakovl.kernel.effect.ZIO
import dev.aliakovl.kernel.effect.ZIO.refineToOrDie

import java.io.IOException

trait Console[F[_]]:
  def printLine(line: => Any): F[Unit]

object Console:
  def apply[F[_]](using c: Console[F]): Console[F] = c

  given Console[[T] =>> ZIO[Any, IOException, T]] with
    def printLine(line: => Any): ZIO[Any, IOException, Unit] = ZIO.attempt(println(line)).refineToOrDie[IOException]
