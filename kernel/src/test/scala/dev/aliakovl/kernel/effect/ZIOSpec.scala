package dev.aliakovl.kernel.effect

enum LengthException:
  case ToShort, ToLong

object ZIOSpec {
  import ZIO.*
  import LengthException.*

  val str = "Hello"

  val program = for {
    n <- ZIO.serviceWith[String](_.length)
//    _ <- ZIO.fail(ToLong)
  } yield n

  def main(args: Array[String]): Unit = {
    val a = program.provide("wef").runZIO

    println(a)
  }

}
