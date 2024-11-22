package dev.aliakovl.gin.macros

import dev.aliakovl.gin.macros.fp.data.State
import dev.aliakovl.gin.macros.fp.optics.Lens

import scala.reflect.macros.whitebox

class StateOps[C <: whitebox.Context with Singleton] {
  type Variables = Map[C#Type, C#TermName]
  type Values = Map[C#Type, C#Tree]
  type VarsState[A] = State[Variables, A]
  case class VState(variables: Variables, values: Values)
  type FullState[A] = State[VState, A]

  object VState {
    implicit def lensForVStateVariables: Lens[VState, Variables] =
      new Lens[VState, Variables] {
        override def get(t: VState): Variables = t.variables
        override def set(t: VState, s: Variables): VState =
          t.copy(variables = s)
      }

    implicit def lensForVStateValues: Lens[VState, Values] =
      new Lens[VState, Values] {
        override def get(t: VState): Values = t.values
        override def set(t: VState, s: Values): VState = t.copy(values = s)
      }
  }
}

object StateOps {
  val dummyContext: whitebox.Context = null
  private val threadLocalStateOps = new StateOps[dummyContext.type]
  def getOps(c: whitebox.Context): StateOps[c.type] = threadLocalStateOps.asInstanceOf[StateOps[c.type]]
  def getOps: StateOps[dummyContext.type] = threadLocalStateOps
}

private[macros] final class Stack[S](
    private val init: S,
    private var states: List[S],
    private var error: Option[String],
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

private[macros] object Stack {
  private val threadLocalStack =
    ThreadLocal.withInitial { () =>
      new Stack(
        init = StateOps.getOps.VState(Map.empty, Map.empty),
        states = List.empty,
        error = None
      )
    }

  def withContext[S, A](
      c: whitebox.Context
  )(f: Stack[S] => c.Expr[A]): c.Expr[A] = {
    val stack: Stack[S] = threadLocalStack.get().asInstanceOf[Stack[S]]
    try f(stack)
    catch {
      case e: Throwable => stack.throwError(e)
    } finally if (stack.isEmpty) {
      threadLocalStack.remove()
    }
  }
}
