package optics

import core.{Applicative, Traverse}
import core.data.Identity

trait Traversal[S, A] {
  def modifyA[F[_]: Applicative](f: A => F[A])(s: S): F[S]
  def modify(f: A => A)(s: S): S = modifyA(a => Identity(f(a)))(s).value
  // = map(_set(_)(s))(Identity(f(_get(s)))).value
  // = Identity(_set(f(_get(s)))(s)).value
  // = _set(f(_get(s)))(s)
  def set(a: A)(s: S): S = modify(_ => a)(s)
  // = _set((_ => a)(_get(s)))(s)
  // = _set(a)(s)
}

object Traversal {
  def fromTraverse[T[_]: Traverse, A]: Traversal[T[A], A] =
    new Traversal[T[A], A] {
      override def modifyA[F[_]: Applicative](
          f: A => F[A]
      )(s: T[A]): F[T[A]] = {
        Traverse[T].traverse(s)(f)
      }
    }

  def apply2[S, A](get1: S => A, get2: S => A)(
      _set: (A, A, S) => S
  ): Traversal[S, A] = new Traversal[S, A] {
    override def modifyA[F[_]: Applicative](f: A => F[A])(s: S): F[S] =
      Applicative[F].map2(f(get1(s)), f(get2(s))) { (a, b) =>
        _set(a, b, s)
      }
  }
}
