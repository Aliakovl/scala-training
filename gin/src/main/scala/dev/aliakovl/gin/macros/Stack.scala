package dev.aliakovl.gin.macros

import scala.reflect.macros.whitebox

final class Stack[C <: whitebox.Context with Singleton] {
  type Variables = Map[C#Type, C#TermName]
  type Values = Map[C#Type, C#Tree]
  type VarsState[A] = State[Variables, A]
  type VState = (Variables, Values)
  type FullState[A] = State[VState, A]

  private var states: List[VState] = List.empty

  def depth: Int = states.size

  def get: List[VState] = states

  private def push(state: VState): Unit = {
    states = state :: states
  }

  private def top(): VState = states.head

  private def pullWithTop(): VState = {
    val top = states.head
    pull()
    pull()
    push(top)
    top
  }

  private def pull(): Unit = {
    states = states.drop(1)
  }

  def withState[A](thunk: => Option[A]): FullState[Option[A]] = State { state =>
    push(state)
    val res = thunk match {
      case Some(value) =>
        val top = pullWithTop()
        (top, Some(value))
      case none =>
        pull()
        (state, none)
    }
    res
  }

  def withStateProvided[A](s: => FullState[Any])(f: VState => A): A = {
    if (states.isEmpty) {
      states = (Map.empty: Variables, Map.empty: Values) :: states
      val s1 = s.eval((Map.empty: Variables, Map.empty: Values))
      val res = f(s1)
      states = List.empty
      res
    } else {
      val s1 = s.eval(top())
      val res = f(s1)
      pull()
      push(s1)
      res
    }
  }

}

object Stack {
  private val dummyContext: whitebox.Context = null
  private val threadLocalStack = ThreadLocal.withInitial[Stack[dummyContext.type]] { () =>
    new Stack[dummyContext.type]
  }

  def withContext[A](c: whitebox.Context)(f: Stack[c.type] => c.Expr[A]): c.Expr[A] = {
    val stack = threadLocalStack.get()
    try f(stack.asInstanceOf[Stack[c.type]])
    finally if (stack.depth <= 0) {
      threadLocalStack.remove()
    }
  }
}