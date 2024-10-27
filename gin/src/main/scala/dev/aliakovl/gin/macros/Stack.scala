package dev.aliakovl.gin.macros

import scala.reflect.macros.whitebox

final class Stack[C <: whitebox.Context with Singleton] {
  private var currentState: Option[Any] = None
  private var depth: Int = 0
  def isEmpty: Boolean = currentState.isEmpty
  def top[A](): Option[A] = currentState.asInstanceOf[Option[A]]
  def set(state: Any): Unit = currentState = Some(state)
  def push(state: Any): Unit = {
    currentState = Some(state)
    depth = depth + 1
  }
  def pop(): Unit = {
    if (depth <= 1) {
      currentState = None
      depth = 0
    } else {
      depth = depth - 1
    }
  }

  def withState[A](thunk: => Option[A]): State[Any, Option[A]] = State { state =>
    push(state)
    try {
      thunk match {
        case None => (state, None)
        case Some(value) => (top().get, Some(value))
      }
    } finally pop()
  }
}

object Stack {
  private val dummyContext: whitebox.Context = null
  private val threadLocalStack = ThreadLocal.withInitial[Stack[dummyContext.type]] { () =>
    new Stack[dummyContext.type]
  }

  def withContext(c: whitebox.Context)(f: Stack[c.type] => c.Tree): c.Tree = {
    val stack = threadLocalStack.get()
    try f(stack.asInstanceOf[Stack[c.type]])
    finally if (stack.depth <= 0) threadLocalStack.remove()
  }
}
