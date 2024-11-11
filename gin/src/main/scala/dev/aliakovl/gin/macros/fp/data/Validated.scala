package dev.aliakovl.gin.macros.fp.data

import dev.aliakovl.gin.macros.fp.data.Validated._
import dev.aliakovl.gin.macros.fp._

sealed abstract class Validated[+E, +A] {
  def fold[B](fe: E => B, fa: A => B): B = this match {
    case Valid(a)   => fa(a)
    case Invalid(e) => fe(e)
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a)       => Valid(f(a))
    case i @ Invalid(_) => i
  }

  def andThen[E1 >: E, B](f: A => Validated[E1, B]): Validated[E1, B] = {
    this match {
      case Valid(a)       => f(a)
      case i @ Invalid(_) => i
    }
  }

  def getOrElse[A1 >: A](f: E => A1): A1 = fold(f, identity)
}

object Validated {
  def valid[E, A](a: A): Validated[E, A] = Valid(a)
  def invalid[E, A](e: E): Validated[E, A] = Invalid(e)

  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]

  implicit def applicativeForValidated[E](implicit
      E: Semigroup[E]
  ): Applicative[({ type V[A] = Validated[E, A] })#V] =
    new Applicative[({ type V[A] = Validated[E, A] })#V] {
      override def pure[A](a: A): Validated[E, A] = Validated.valid(a)

      override def ap[A, B](ff: Validated[E, A => B])(
          fa: Validated[E, A]
      ): Validated[E, B] = (ff, fa) match {
        case (Valid(f), Valid(a))       => Valid(f(a))
        case (Valid(_), Invalid(e))     => Invalid(e)
        case (Invalid(e), Valid(_))     => Invalid(e)
        case (Invalid(e1), Invalid(e2)) => Invalid(E.combine(e1, e2))
      }
    }

  implicit def traverseForValidated[E]
      : Traverse[({ type V[A] = Validated[E, A] })#V] =
    new Traverse[({ type V[A] = Validated[E, A] })#V] {
      override def traverse[F[_], A, B](ta: Validated[E, A])(
          f: A => F[B]
      )(implicit F: Applicative[F]): F[Validated[E, B]] = ta match {
        case Valid(a)       => F.map(f(a))(Valid.apply)
        case i @ Invalid(_) => F.pure(i)
      }
    }
}
