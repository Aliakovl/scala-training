package dev.aliakovl.monad.data

trait Group[A] { self =>
  def combine(a: A, b: A): A
  def identity: A
  def inverse(a: A): A
  def modify[B](iso: A ~ B): Group[B] = new Group[B]:
    override def combine(a: B, b: B): B =
      iso.f(self.combine(iso.g(a), iso.g(b)))
    override def identity: B = iso.f(self.identity)
    override def inverse(a: B): B = iso.f(self.inverse(iso.g(a)))
}

case class Sum[A: Numeric](value: A)
case class Mul[A: Fractional](value: A)

object Group:
  inline def apply[A](using Group[A]): Group[A] = summon[Group[A]]

  extension[A: Group](a: A)
    def combine(b: A): A = Group[A].combine(a, b)
    def inverse: A = Group[A].inverse(a)

  given [A, B](using A ~ B): Conversion[Group[A], Group[B]] with
    override def apply(x: Group[A]): Group[B] = x.modify(summon[A ~ B])

  given[A: Numeric]: Group[Sum[A]] with
    import scala.math.Numeric.Implicits.given
    override def combine(a: Sum[A], b: Sum[A]): Sum[A] = Sum(a.value + b.value)
    override def identity: Sum[A] = Sum(Numeric[A].zero)
    override def inverse(a: Sum[A]): Sum[A] = Sum(-a.value)

  given [A: Fractional]: Group[Mul[A]] with
    import scala.math.Fractional.Implicits.given
    override def combine(a: Mul[A], b: Mul[A]): Mul[A] = Mul(a.value * b.value)
    override def identity: Mul[A] = Mul(Numeric[A].one)
    override def inverse(a: Mul[A]): Mul[A] = Mul(Numeric[A].one / a.value)
