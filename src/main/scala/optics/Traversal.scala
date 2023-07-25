package optics

import core.{Applicative, Traverse}
import core.data.Identity

trait Traversal[S, A] {
  def modifyF[F[_]: Applicative](f: A => F[A])(s: S): F[S]
  def modify(f: A => A)(s: S): S = modifyF(a => Identity(f(a)))(s).value
  def set(a: A)(s: S): S = modify(_ => a)(s)
}

object Traversal {
  def fromTraverse[T[_]: Traverse, A]: Traversal[T[A], A] =
    new Traversal[T[A], A] {
      override def modifyF[F[_]: Applicative](
          f: A => F[A]
      )(s: T[A]): F[T[A]] = {
        Traverse[T].traverse(s)(f)
      }
    }

  def apply2[S, A](get1: S => A, get2: S => A)(
      _set: (A, A, S) => S
  ): Traversal[S, A] = new Traversal[S, A] {
    override def modifyF[F[_]: Applicative](f: A => F[A])(s: S): F[S] =
      Applicative[F].map2(f(get1(s)), f(get2(s))) { (a, b) =>
        _set(a, b, s)
      }
  }
}
