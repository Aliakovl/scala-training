package dev.aliakovl.monad

import dev.aliakovl.monad.data.*

trait IsoMonad[M[_]] extends Monad[~, M]:
  override def cat: Category[~] = summon[Category[~]]

object IsoMonad:
  inline def apply[M[_]](using IsoMonad[M]): IsoMonad[M] = summon[IsoMonad[M]]

  given IsoMonad[Id] with
    override def pure[A]: A ~ Id[A] = Iso(Id(_), _.value)
    override def flatten[A]: Id[Id[A]] ~ Id[A] = Iso(_.value, Id(_))
    override def map[A, B](f: Iso[A, B]): Id[A] ~ Id[B] = Iso(
      ida => Id(f.f(ida.value)),
      idb => Id(f.g(idb.value))
    )
