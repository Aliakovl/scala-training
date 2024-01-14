package dev.aliakovl.monad

import dev.aliakovl.monad.data.Id

trait FunctionMonad[M[_]] extends Monad[Function, M]:
  override def cat: Category[Function] = summon[Category[Function]]

object FunctionMonad:
  inline def apply[M[_]](using FunctionMonad[M]): FunctionMonad[M] =
    summon[FunctionMonad[M]]

  extension [M[_], A](ma: M[A])(using FunctionMonad[M])
    def map[B](f: A => B): M[B] = FunctionMonad[M].map(f)(ma)
    def flatMap[B](f: A => M[B]): M[B] = FunctionMonad[M].flatMap(f)(ma)

  given FunctionMonad[Id] with
    override def pure[A]: Function[A, Id[A]] = Id(_)
    override def flatten[A]: Function[Id[Id[A]], Id[A]] = _.value
    override def map[A, B](f: Function[A, B]): Function[Id[A], Id[B]] = ida =>
      Id(f(ida.value))
