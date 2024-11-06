package dev.aliakovl.gin.macros

import scala.reflect.macros.whitebox

final class Stack[C <: whitebox.Context with Singleton] {
  type Variables = Map[C#Type, C#TermName]
  type Values = Map[C#Type, C#Tree]
  type VarsState[A] = State[Variables, A]
  type VState = (Variables, Values)
  type FullState[A] = State[VState, A]

  private val init: VState = (Map.empty, Map.empty)
  private var states: List[VState] = List.empty

  def depth: Int = states.size

  private def push(state: VState): Unit = {
    states = state :: states
  }

  private def top(): Option[VState] = states.headOption

  private def pullWithTop(): VState = {
    top().fold(init) { top =>
      pull()
      pull()
      push(top)
      top
    }
  }

  private def pull(): Unit = {
    states = states.drop(1)
  }

  def withState[A](thunk: => Either[String, A]): FullState[Either[String, A]] = State { state =>
    push(state)
    thunk match {
      case success@Right(_) =>
        val top = pullWithTop()
        (top, success)
      case failure@Left(_) =>
        pull()
        (state, failure)
    }
  }

  def withStateProvided[A](state: => FullState[Any])(f: VState => A): A = {
    def evaluate(from: VState): (VState, A) = {
      val to = state.eval(from)
      (to, f(to))
    }

    top().fold {
      states = init :: states
      val value = evaluate(init)._2
      states = List.empty
      value
    } { state =>
      val (newState, value) = evaluate(state)
      pull()
      push(newState)
      value
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
    catch {
      case e: Throwable =>
        stack.states = List.empty
        throw e
    }
    finally if (stack.depth <= 0) {
      threadLocalStack.remove()
    }
  }
}
