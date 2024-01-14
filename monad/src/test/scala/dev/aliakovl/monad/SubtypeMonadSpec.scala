package dev.aliakovl.monad

import dev.aliakovl.monad.data.Res
import dev.aliakovl.monad.SubtypeMonad.*

object SubtypeMonadSpec:
  val a: Error | Res[Int] = 3

  val b: Res[Int] = SubtypeMonad[Res].flatten[Int](a)

  def main(args: Array[String]): Unit = {
    println(b)
  }
