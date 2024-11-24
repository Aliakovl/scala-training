package dev.aliakovl.gin.macros

import dev.aliakovl.gin.macros.fp.data.State

import scala.reflect.macros.whitebox

private[macros] final class Stack[S](
    private val init: S,
    private var states: List[S],
    private var error: Option[String]
) {
  private def push(state: S): Unit = {
    states = state :: states
  }

  private def top(): Option[S] = states.headOption

  private def pullWithTop(): S = {
    top().fold(init) { top =>
      pop()
      pop()
      push(top)
      top
    }
  }

  private def pop(): Unit = {
    states = states.drop(1)
  }

  private def throwError(e: Throwable): Nothing = {
    error = Some(e.getMessage)
    states = List.empty
    throw e
  }

  def isEmpty: Boolean = states.isEmpty

  def depth: Int = states.size

  def statefulSearch[A](
      thunk: => Either[String, A]
  ): State[S, Either[String, A]] = State { state =>
    push(state)
    thunk match {
      case Left(msg) =>
        pop()
        (state, Left(error.fold(msg)(reason => s"$msg\nReason: $reason")))
      case success =>
        val top = pullWithTop()
        (top, success)
    }
  }

  def withStateProvided[A](state: => State[S, Any])(f: S => A): A = {
    def evaluate(from: S): (S, A) = {
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
      pop()
      push(newState)
      value
    }
  }
}

private[macros] object Stack extends StateMacros {
  private val dummyContext: whitebox.Context = null

  val c: whitebox.Context = dummyContext

  private val threadLocalStack = ThreadLocal.withInitial { () =>
    new Stack(
      init = VState(Map.empty, Map.empty),
      states = List.empty,
      error = None
    )
  }

  def withContext[S <: StateMacros#VState, A](
      c: whitebox.Context
  )(f: Stack[S] => c.Expr[A]): c.Expr[A] = {
    val stack = threadLocalStack.get().asInstanceOf[Stack[S]]
    try f(stack)
    catch {
      case e: Throwable => stack.throwError(e)
    } finally if (stack.isEmpty) {
      threadLocalStack.remove()
    }
  }
}
