package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy, Nat, ops}

import scala.util.{Random => ScalaRandom}

trait MkCoproductRandom[C <: Coproduct] {
  def get(): C
}

object MkCoproductRandom {
  implicit val cNilGenerator: MkCoproductRandom[CNil] =
    new MkCoproductRandom[CNil] {
      override def get(): CNil = throw new RuntimeException("CNil")
    }

  implicit def cConsGenerator[H, T <: Coproduct, N <: Nat](implicit
      head: Lazy[Random[H]],
      tail: MkCoproductRandom[T],
      length: ops.coproduct.Length.Aux[T, N],
      n: ops.nat.ToInt[N]
  ): MkCoproductRandom[H :+: T] = new MkCoproductRandom[H :+: T] {
    override def get(): H :+: T =
      if (ScalaRandom.nextInt(n() + 1) == 0) Inl(head.value.get())
      else Inr(tail.get())
  }
}
