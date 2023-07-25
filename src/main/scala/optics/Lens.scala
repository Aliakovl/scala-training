package optics

import core.{Applicative, Functor}
import core.data._

import scala.language.existentials

trait Lens[S, A] extends Traversal[S, A] {
  def get(s: S): A = modifyF(Const[A, A])(s).value

  def modifyF[F[_]: Functor](f: A => F[A])(s: S): F[S]

  override def modifyA[F[_]: Applicative](f: A => F[A])(s: S): F[S] =
    modifyF(f)(s)
}

object Lens {
  def apply[S, A](_get: S => A)(_set: A => S => S): Lens[S, A] =
    new Lens[S, A] {
      override def modifyF[F[_]: Functor](f: A => F[A])(s: S): F[S] =
        Functor[F].map[A, S](_set(_)(s))(f(_get(s)))
    }

  def composeLens[S, A, B](l1: Lens[S, A], l2: Lens[A, B]): Lens[S, B] =
    new Lens[S, B] {
      override def modifyF[F[_]: Functor](f: B => F[B])(s: S): F[S] = {
        l1.modifyF(l2.modifyF(f))(s)
      }
    }

  implicit class LensSyntax[S, A](private val self: Lens[S, A]) extends AnyVal {
    def composeLens[B](other: Lens[A, B]): Lens[S, B] =
      Lens.composeLens(self, other)

    def ^|->[B](other: Lens[A, B]): Lens[S, B] = {
      Lens.composeLens(self, other)
    }
  }
}
