package dev.aliakovl.meta.macros

import Macros.*

import scala.io.StdIn

def func(a: Int): Int = a * a

@main
def demo(): Unit =
  println(blub)
  val str: String = "qwefqwef"
  println(length(str))

  println(reify {
    val a: Int = 3
    def g: String = "wef"
  })

  val p: Int = 3

  val res = power (
    {
      val a = "RRer"
      println(a)
      p
    }, 4
  )

  println(res)

  println(test)

  def g(x: String): Int = {
    println(s"g: $x")
    x.toInt
  }

  def f(x: Int): Int = {
    println(s"f: $x")
    x + 1
  }

  def h(x: Int): Int = {
    println(s"h: $x")
    x + 1
  }

  def y(x: Int): Int = {
    println(s"y: $x")
    x + 1
  }

  fuseMapCode(
    List("4", "7", "2").map(g).map(f).map(h).map(y)
  )
