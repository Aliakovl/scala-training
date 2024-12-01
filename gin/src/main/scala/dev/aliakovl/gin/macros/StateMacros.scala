package dev.aliakovl.gin.macros

import dev.aliakovl.gin.macros.fp.data.State
import dev.aliakovl.gin.macros.fp.optics.Lens

import scala.language.implicitConversions
import scala.reflect.macros.whitebox

private[macros] trait StateMacros {
  val c: whitebox.Context

  class WrappedType(val tpe: c.Type) {
    override def equals(obj: Any): Boolean = {
      obj match {
        case WrappedType(o) => tpe =:= o
        case _ => false
      }
    }

    override def toString: String = tpe.toString
  }

  object WrappedType {
    def apply(tpe: c.Type) = new WrappedType(tpe)
    def unapply(wt: WrappedType): Option[c.Type] = Some(wt.tpe)
  }

  implicit class WrappedTypeOps(private val value: c.Type) {
    def wrap: WrappedType = WrappedType(value)
  }

//  implicit def wrapType(tpe: c.Type): WrappedType = WrappedType(tpe)

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
