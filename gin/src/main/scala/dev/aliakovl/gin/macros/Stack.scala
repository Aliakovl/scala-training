package dev.aliakovl.gin.macros

import scala.reflect.macros.blackbox

final class Stack[C <: blackbox.Context] {
  private var frames: List[C#Type] = List.empty[C#Type]
  def state: List[C#Type] = frames
  def push(x: C#Type): Unit = frames ::= x
  def pop(): Unit = frames = frames drop 1
}

object Stack {
  private val threadLocalStack = ThreadLocal.withInitial[Stack[blackbox.Context]] { () =>
    new Stack[blackbox.Context]
  }
  def withContext(c: blackbox.Context)(f: Stack[c.type] => c.Tree): c.Tree = {
    val stack = threadLocalStack.get()
    f(stack.asInstanceOf[Stack[c.type]])
  }
}
