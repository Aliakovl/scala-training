package dev.aliakovl.shapelessguide

import cats.Monoid
import shapeless._
import shapeless.ops.hlist._

trait Migration[A, B] {
  def apply(a: A): B
}

object Migration {
  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }

  implicit def genericMigration[
      A,
      B,
      ARepr <: HList,
      BRepr <: HList,
      Common <: HList,
      Added <: HList,
      Unaligned <: HList
  ](implicit
      aGen: LabelledGeneric.Aux[A, ARepr],
      bGen: LabelledGeneric.Aux[B, BRepr],
      inter: Intersection.Aux[ARepr, BRepr, Common],
      diff: Diff.Aux[BRepr, Common, Added],
      monoid: Monoid[Added],
      prepend: Prepend.Aux[Added, Common, Unaligned],
      align: Align[Unaligned, BRepr]
  ): Migration[A, B] =
    new Migration[A, B] {
      def apply(a: A): B =
        bGen.from(align(prepend(monoid.empty, inter(aGen.to(a)))))
    }
}
