package dev.aliakovl.gin.macros

import dev.aliakovl.gin.macros.fp.data.State
import dev.aliakovl.gin.macros.fp.optics.Lens

import scala.reflect.macros.whitebox

private[macros] trait StateMacros {
  val c: whitebox.Context

  type Variables = Map[c.Type, c.TermName]
  type Values = Map[c.Type, c.Tree]
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
