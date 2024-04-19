package dev.aliakovl.shapelessguide

import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt
import shapeless.{Generic, HList, Nat}

trait SizeOf[A] {
  def value: Int
}

object SizeOf {
  def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

  implicit def genericSizeOf[A, L <: HList, N <: Nat](implicit
      gen: Generic.Aux[A, L],
      length: Length.Aux[L, N],
      sizeToInt: ToInt[N]
  ): SizeOf[A] = new SizeOf[A] {
    override def value: Int = sizeToInt.apply()
  }
}
