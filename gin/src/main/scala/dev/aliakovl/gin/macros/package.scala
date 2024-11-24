package dev.aliakovl.gin

import scala.reflect.macros.whitebox

package object macros {
  implicit final class DebugOps[A](private val value: A) extends AnyVal {
    def debug[C <: whitebox.Context](c: C): A = {
      val message = s"${DebugOps.next()}\t#${Thread.currentThread().getId}#${Thread.currentThread().getStackTrace()(2)}: $value"
      c.info(c.enclosingPosition, message, force = false)
      value
    }
  }

  private object DebugOps {
    private val counter: ThreadLocal[Int] = ThreadLocal.withInitial[Int](() => 0)
    private def next(): Int = {
      val res = counter.get()
      counter.set(res + 1)
      res
    }
  }
}
