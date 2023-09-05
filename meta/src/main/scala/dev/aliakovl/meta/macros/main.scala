package dev.aliakovl.meta.macros


import Macros.{blub, length}

@main
def demo(): Unit =
  println(blub)
  val str: String = "qwefqwef"
  println(length(str))
