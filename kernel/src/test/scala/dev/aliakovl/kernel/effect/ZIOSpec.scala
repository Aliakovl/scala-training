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

  def fib(n: Int): ZIO[Any, IOException, Int] = if n <= 1 then
    ZIO.success(n)
  else for {
    a <- ZIO.suspendSucceed(fib(n - 1))
    b <- fib(n - 2)
    _ <- ZIO.printLine(a + b)
  } yield a + b

  def main(args: Array[String]): Unit = (for {
    _ <- program
    _ <- fib(10).flatMap(ZIO.printLine(_))
  } yield ()).run()
}
