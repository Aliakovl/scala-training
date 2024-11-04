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

  private def push(state: VState): Unit = {
    states = state :: states
  }

  private def top(): VState = states.head

  private def pullWithTop(): VState = {
    val top = states.head
    states = states.tail match {
      case _ :: tail => NonEmptyList(top, tail)
      case Nil => states
    }
    top
  }

  private def pull(): VState = {
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
        val top = pullWithTop()
        (top, Some(value))
      case none =>
        pull()
        (state, none)
    }
    res
  }

  def withStateProvided[A](s: => FullState[Any])(f: VState => A): A = {
    val s1 = s.eval(top())
    val res = f(s1)
    if (states.tail.nonEmpty) {
      pull()
      push(s1)
    }
    res
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
    finally if (stack.depth <= 1) {
      threadLocalStack.remove()
    }
  }
}