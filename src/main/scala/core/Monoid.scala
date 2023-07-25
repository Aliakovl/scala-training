package core

import core.Semigroup.SemigroupOps
import core.data.Const

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x <> y
  }

  def optionMonoid[A: Semigroup]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def empty: Option[A] = None

      override def combine(x: Option[A], y: Option[A]): Option[A] = {
        (x, y) match {
          case (Some(_x), Some(_y)) => Some(_x <> _y)
          case (Some(_x), None)     => Some(_x)
          case (None, Some(_y))     => Some(_y)
          case _                    => None
        }
      }
    }

  implicit def firstOption[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None

      def combine(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
    }

  implicit def constMonoid[A: Monoid, B]: Monoid[Const[A, B]] =
    new Monoid[Const[A, B]] {
      override def empty: Const[A, B] = Const(implicitly[Monoid[A]].empty)

      override def combine(x: Const[A, B], y: Const[A, B]): Const[A, B] = Const(
        x.value <> y.value
      )
    }
}
