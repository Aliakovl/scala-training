package dev.aliakovl.gin

import scala.reflect.macros.whitebox

package object macros {

  implicit class DebugOps[A](private val a: A) extends AnyVal {
    def debug[C <: whitebox.Context](c: C): A = {
      println(s"${DebugOps.next()}\t#${Thread.currentThread().getId}#${Thread.currentThread().getStackTrace()(2)}|${c.enclosingPosition} $a")
      a
    }
  }

  object DebugOps {
    private val counter: ThreadLocal[Int] = ThreadLocal.withInitial[Int](() => 0)
    private def next(): Int = {
      val res = counter.get()
      counter.set(res + 1)
      res
    }
  }
}


