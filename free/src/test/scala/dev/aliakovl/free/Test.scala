package dev.aliakovl.free

import DBOps._

object Test {
  val program: IO[Int] = (for {
    _ <- create("num", 5)
    n <- read[Int]("num")
  } yield n).foldMap[IO]

  def main(args: Array[String]): Unit = {
    program.unsafeRun()
  }
}
