package dev.aliakovl.core.data

sealed trait HList extends Product with Serializable

object HList {
  implicit class HListOpt[T <: HList](private val tail: T) extends AnyVal {
    def ::[H](head: H): ::[H, T] = new ::(head, tail)
  }

  type HNil = HNil.type
}

final case class ::[+H, +T <: HList](head: H, tail: T) extends HList

case object HNil extends HList
