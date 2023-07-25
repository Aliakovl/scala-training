package dev.aliakovl.optics

import dev.aliakovl.core.Applicative
import dev.aliakovl.core.data.Const

trait Prism[S, A] extends Traversal[S, A] {
  def getOrModify(s: S): Either[S, A] = getOption(s) match {
    case Some(value) => Right(value)
    case None        => Left(s)
  }

  def getOption(s: S): Option[A] =
    modifyA(a => Const[Option[A], A](Some(a)))(s).value

  def reverseGet(a: A): S

  def modifyA[F[_]: Applicative](f: A => F[A])(s: S): F[S]
}

object Prism {
  def apply[S, A](
      _getOption: S => Option[A]
  )(_reverseGet: A => S): Prism[S, A] = new Prism[S, A] {
    override def reverseGet(a: A): S = _reverseGet(a)

    override def modifyA[F[_]: Applicative](f: A => F[A])(s: S): F[S] = {
      _getOption(s).fold(
        Applicative[F].pure(s)
      )(a => Applicative[F].map(_reverseGet)(f(a)))
    }
  }
}
