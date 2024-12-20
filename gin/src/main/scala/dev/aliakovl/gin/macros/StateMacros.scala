package dev.aliakovl.gin.macros

import dev.aliakovl.gin.macros.fp.data.State
import dev.aliakovl.gin.macros.fp.optics.Lens

import scala.reflect.macros.whitebox

private[macros] trait StateMacros {
  val c: whitebox.Context

  class WrappedType(val tpe: c.Type) {
    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[WrappedType]) {
        obj.asInstanceOf[WrappedType].tpe =:= tpe
      } else {
        false
      }
    }

    override def toString: String = tpe.toString
  }

  object WrappedType {
    def apply(tpe: c.Type) = new WrappedType(tpe)
  }

  implicit class WrappedTypeOps(private val value: c.Type) {
    def wrap: WrappedType = WrappedType(value)
  }

  type Variables = Map[WrappedType, c.TermName]
  type Values = Map[WrappedType, c.Tree]
  type VarsState[A] = State[Variables, A]
  case class VState(variables: Variables, values: Values)
  type FullState[A] = State[VState, A]

  object VState {
    implicit def lensForVStateVariables: Lens[VState, Variables] =
      Lens[VState, Variables](_.variables)(state =>
        variables => state.copy(variables = variables)
      )

    implicit def lensForVStateValues: Lens[VState, Values] =
      Lens[VState, Values](_.values)(state =>
        values => state.copy(values = values)
      )
  }
}
