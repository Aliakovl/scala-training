package dev.aliakovl.kernel

import instances.given
import Compose.given

object ComposeSpec:

  val a: Either[String, Option[Int]] = Right(Some(3))
  val b: Either[String, Option[Int]] = Left("kjhwef")

  def main(args: Array[String]): Unit = {
    val c = Applicative[[T] =>> Either[String, Option[T]]].zip(a, b)
    println(c)
  }
