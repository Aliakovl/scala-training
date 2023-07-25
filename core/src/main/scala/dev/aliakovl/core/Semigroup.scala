package dev.aliakovl.core

import dev.aliakovl.core.Monoid._

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A](implicit ev: Semigroup[A]): Semigroup[A] = ev

  implicit class SemigroupOps[A](private val x: A)(implicit
      inst: Semigroup[A]
  ) {
    def combine(y: A): A = inst.combine(x, y)
    def <>(y: A): A = inst.combine(x, y)
  }

  implicit val stringSemigroup: Semigroup[String] = stringMonoid

  implicit def optionSemigroup[A: Semigroup]: Semigroup[Option[A]] =
    optionMonoid[A]

  implicit def functionSemigroup[A, B: Semigroup]: Semigroup[A => B] =
    (f: A => B, g: A => B) => a => f(a) <> g(a)
}
