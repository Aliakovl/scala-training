package dev.aliakovl.gin

package object macros {

  implicit class DebugOps[A](private val a: A) extends AnyVal {
    def debug: A = {
      println(s"${DebugOps.next()}#${Thread.currentThread().getStackTrace()(2)}: $a")
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


