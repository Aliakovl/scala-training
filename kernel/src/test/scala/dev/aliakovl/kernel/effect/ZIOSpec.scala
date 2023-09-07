package dev.aliakovl.kernel.effect

import ZIO.*
import LengthException.*

import java.io.IOException

enum LengthException:
  case ToShort, ToLong

object ZIOSpec {
  val str = "Hello"

  def rightString(min: Int, max: Int): ZIO[String, LengthException, Int] = for {
    n <- ZIO.serviceWith[String](_.length)
    _ <- if n >= max then
      ZIO.fail(ToLong)
    else if n <= min then
      ZIO.fail(ToShort)
    else ZIO.success(())
  } yield n

  val program: ZIO[Any, IOException, Unit] = rightString(1, 6)
    .catchAll(e => ZIO.success(e.toString)).flatMap { a =>
      ZIO.printLine(a)
    }
    .provide(str)

  def main(args: Array[String]): Unit = {
    program.run
  }
}
