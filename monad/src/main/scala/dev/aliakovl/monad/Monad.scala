package dev.aliakovl.monad

trait Monad[->[_, _], M[_]]:
  given cat: Category[->]
  def map[A, B](f: A -> B): M[A] -> M[B] = flatMap(cat.andThen(f, pure))
  def pure[A]: A -> M[A]
  def flatten[A]: M[M[A]] -> M[A] = flatMap(cat.id)
  def flatMap[A, B](f: A -> M[B]): M[A] -> M[B] = cat.andThen(map(f), flatten)

object Monad:
  inline def apply[->[_, _], M[_]](using Monad[->, M]): Monad[->, M] =
    summon[Monad[->, M]]

  extension [->[_, _], M[_], A, B](f: A -> M[B])(using Monad[->, M])
    def >=>[C](g: B -> M[C]): A -> M[C] = {
      Monad[->, M].cat.andThen(f, Monad[->, M].flatMap(g))
    }
