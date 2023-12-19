package dev.aliakovl.kernel

import Compose.given

object ComposeSpec:
  val a: Either[String, Option[Int]] = Right(Some(3))
  val b: Either[String, Option[Int]] = Left("kjhwef")

  def main(args: Array[String]): Unit = {
    val c = summon[Applicative[[T] =>> Either[String, Option[T]]]].zip(a, b)
    println(c)
    val p = Applicative.pure[Option](4)
    print(p)
  }
