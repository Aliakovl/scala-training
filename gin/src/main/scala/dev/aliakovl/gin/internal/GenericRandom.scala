package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import shapeless.{Coproduct, Generic, HList, Lazy}

trait GenericRandom {
  implicit def fromMkHListGenerator[A, L <: HList](implicit
      gen: Generic.Aux[A, L],
      hGen: Lazy[MkHListRandom[L]]
  ): Random[A] = Random(gen.from(hGen.value.get()))

  implicit def fromMkCoproductGenerator[A, C <: Coproduct](implicit
      gen: Generic.Aux[A, C],
      cGen: Lazy[MkCoproductRandom[C]]
  ): Random[A] = Random(gen.from(cGen.value.get()))
}
