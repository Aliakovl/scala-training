package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import shapeless.{::, HList, HNil, Lazy}

trait MkHListRandom[L <: HList] {
  def get(): L
}

object MkHListRandom {
  implicit val hNilGenerator: MkHListRandom[HNil] = new MkHListRandom[HNil] {
    override def get(): HNil = HNil
  }

  implicit def hConsGenerator[H, T <: HList](implicit
      head: Lazy[Random[H]],
      tail: MkHListRandom[T]
  ): MkHListRandom[H :: T] = new MkHListRandom[H :: T] {
    override def get(): H :: T = head.value.get() :: tail.get()
  }
}
