package dev.aliakovl.monad

import dev.aliakovl.monad.data.{<=, Id, Op}
import dev.aliakovl.monad.CoMonad.given
import dev.aliakovl.monad.Monad.>=>

object CoMonadSpec:
  val f: String <= Id[Boolean] = Op { case Id(b) =>
    println("f"); String.valueOf(b)
  }
  val g: Boolean <= Id[Int] = Op { case Id(i) => println("g"); i % 2 == 0 }
  val h: Int <= Id[List[Int]] = Op { case Id(xs) => println("h"); xs.sum }

  val result: String = (f >=> g >=> h).run(Id(List(2, 1)))

  def main(args: Array[String]): Unit = {
    println(result)
  }
