package core

import core.Semigroup.SemigroupOps
import core.Traverse.listTraverse

trait Foldable[T[_]] {
  def fold[M: Monoid](tm: T[M]): M = foldMap[M, M](tm)(identity)

  def foldMap[A, M: Monoid](ta: T[A])(f: A => M): M =
    foldLeft[A, M](ta, Monoid[M].empty)((m, a) => m <> f(a))

  def foldRight[A, B](ta: T[A], ini: B)(f: (A, B) => B): B

  def foldLeft[A, B](ta: T[A], ini: B)(f: (B, A) => B): B
}

object Foldable {
  def apply[T[_]](implicit inst: Foldable[T]): Foldable[T] = inst

  implicit class FoldableOps[T[_], A](private val ta: T[A])(implicit
      inst: Foldable[T]
  ) {
    def fold[M: Monoid](implicit ev: A <:< M): M =
      inst.fold(ta.asInstanceOf[T[M]])
    def foldMap[M: Monoid](f: A => M): M = inst.foldMap(ta)(f)
    def foldRight[B](ini: B)(f: (A, B) => B): B = inst.foldRight(ta, ini)(f)
    def foldLeft[B](ini: B)(f: (B, A) => B): B = inst.foldLeft(ta, ini)(f)
  }

  implicit val listFoldable: Foldable[List] = listTraverse
}
