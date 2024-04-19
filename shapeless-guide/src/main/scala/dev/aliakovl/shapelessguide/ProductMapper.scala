package dev.aliakovl.shapelessguide

import shapeless.ops.hlist.Mapper
import shapeless.{Generic, HList, Poly}

trait ProductMapper[A, B, P] {
  def apply(a: A): B
}

object ProductMapper {
  implicit def genericProductMapper[
      A,
      B,
      P <: Poly,
      ARepr <: HList,
      BRepr <: HList
  ](implicit
      aGen: Generic.Aux[A, ARepr],
      bGen: Generic.Aux[B, BRepr],
      mapper: Mapper.Aux[P, ARepr, BRepr]
  ): ProductMapper[A, B, P] = new ProductMapper[A, B, P] {
    override def apply(a: A): B = bGen.from(mapper(aGen.to(a)))
  }

  implicit class ProductMapperOps[A](a: A) {
    class Builder[B] {
      def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A, B, P]): B =
        pm(a)
    }

    def mapTo[B]: Builder[B] = new Builder[B]
  }
}
