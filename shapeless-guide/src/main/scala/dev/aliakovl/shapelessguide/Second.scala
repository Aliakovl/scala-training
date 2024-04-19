package dev.aliakovl.shapelessguide

import shapeless._

trait Second[L <: HList] {
  type Out
  def apply(value: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] { type Out = O }

  def apply[L <: HList](implicit inst: Second[L]): Second.Aux[L, inst.Out] = inst

  implicit def hlistSecond[A, B, Rest <: HList]: Second.Aux[A :: B :: Rest, B] = new Second[A :: B :: Rest] {
    type Out = B
    override def apply(value: A :: B :: Rest): B = value.tail.head
  }
}
