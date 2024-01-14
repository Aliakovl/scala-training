package dev.aliakovl.monad

import dev.aliakovl.monad.data.Res

trait SubtypeMonad[M[_]] extends Monad[<:<, M]:
  override def cat: Category[<:<] = summon[Category[<:<]]

object SubtypeMonad:
  inline def apply[M[_]](using SubtypeMonad[M]): SubtypeMonad[M] =
    summon[SubtypeMonad[M]]

  extension [M[_]: SubtypeMonad, A](ma: M[A])
    def map[B](f: A <:< B): M[B] = SubtypeMonad[M].map(f)(ma)
    def flatMap[B](f: A <:< M[B]): M[B] = SubtypeMonad[M].flatMap(f)(ma)

  extension [M[_]: SubtypeMonad, A](ma: M[M[A]])
    def flatten: M[A] = SubtypeMonad[M].flatten[A](ma)

  given SubtypeMonad[Res] with
    override def pure[A]: A <:< Res[A] = summon[A <:< Res[A]]
    override def map[A, B](f: A <:< B): Res[A] <:< Res[B] = f.liftCo[Res]
    override def flatten[A]: Res[Res[A]] <:< Res[A] =
      summon[(Error | Error | A) <:< (Error | A)]
