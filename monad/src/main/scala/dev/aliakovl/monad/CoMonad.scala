package dev.aliakovl.monad

import dev.aliakovl.monad.data.{<=, Id, Op}

trait CoMonad[M[_]] extends Monad[<=, M]:
  override def cat: Category[<=] = summon[Category[<=]]

object CoMonad:
  inline def apply[M[_]](using CoMonad[M]): CoMonad[M] = summon[CoMonad[M]]

  extension [M[_], B](mb: M[B])(using CoMonad[M])
    def map[A](f: A <= B): M[A] = CoMonad[M].map(f).run(mb)
    def flatMap[A](f: A <= M[B]): M[A] = CoMonad[M].flatMap(f).run(mb)

  given CoMonad[Id] with
    override def pure[A]: A <= Id[A] = Op(_.value)
    override def flatten[A]: Id[Id[A]] <= Id[A] = Op(Id(_))
    override def map[A, B](f: A <= B): Id[A] <= Id[B] =
      Op(idB => Id(f.run(idB.value)))
