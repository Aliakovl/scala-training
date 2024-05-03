package dev.aliakovl.gin

import shapeless._

sealed trait GenRule[A]
case class RandomRule[A]() extends GenRule[A]
case class ConstRule[A](value: A) extends GenRule[A]

case class UserRule(
    age: GenRule[Int],
    name: GenRule[String],
    sex: GenRule[Option[GenRule[Sex]]]
)

object GenRule {

  implicit def f[A]: Random[GenRule[A]] = () => RandomRule()

  type UR = GenRule[UserRule]
  type URI = GenRule[Int] :: GenRule[String] :: GenRule[Option[GenRule[Sex]]] ::
    HNil

  def gen[A](v: GenRule[A])(implicit r: Random[A]): A = {
    v match {
      case RandomRule()     => r.apply()
      case ConstRule(value) => value
    }
  }

  def gen[H, T <: HList, Out](v: GenRule[H] :: T)(implicit
      r: Random[H],
      mk: MkGen.Aux[T, Out]
  ): H :: Out = {
    v match {
      case h :: t =>
        h match {
          case RandomRule()     => r.apply() :: gen(t)
          case ConstRule(value) => value :: gen(t)
        }
    }
  }

  def gen(v: HNil): HNil.type = HNil

  def apply[A](implicit genRule: GenRule[A]): GenRule[A] = genRule
}

trait MkGen[A] {
  type H
  def apply(g: A): H
}

object MkGen {
  type Aux[A, Out] = MkGen[A] { type O = Out }
}
