package dev.aliakovl.gin.macros

import cats.data.NonEmptyList

import scala.reflect.macros.whitebox

final class Stack[C <: whitebox.Context with Singleton] {
  type Variables = Map[C#Type, C#TermName]
  type Values = Map[C#Type, C#Tree]
  type VarsState[A] = State[Variables, A]
  type VState = (Variables, Values)
  type FullState[A] = State[VState, A]

  private var states: NonEmptyList[VState] = NonEmptyList.one((Map.empty, Map.empty))

  def depth: Int = states.size

  def push(state: VState): Unit = {
    states = state :: states
  }

  def top(): VState = states.head

  def pull(): VState = {
    val top = states.head
    states = states.tail match {
      case head :: tail => NonEmptyList(head, tail)
      case Nil => states
    }
    top
  }

  def withState[A](thunk: => Option[A]): FullState[Option[A]] = State { state =>
    push(state)
    val res = thunk match {
      case Some(value) =>
        (top(), Some(value))
      case none => (state, none)
    }
    pull()
    res
  }

  def ff[A](s: FullState[Any])(f: VState => A): A = f(s.eval(top()))

}

object Stack {
  private val dummyContext: whitebox.Context = null
  private val threadLocalStack = ThreadLocal.withInitial[Stack[dummyContext.type]] { () =>
    new Stack[dummyContext.type]
  }

  def withContext[A](c: whitebox.Context)(f: Stack[c.type] => c.Expr[A]): c.Expr[A] = {
    val stack = threadLocalStack.get()
    try f(stack.asInstanceOf[Stack[c.type]])
    finally if (stack.depth <= 1) threadLocalStack.remove()
  }
}