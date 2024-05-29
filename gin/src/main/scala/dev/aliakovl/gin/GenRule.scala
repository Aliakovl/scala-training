package dev.aliakovl.gin

sealed trait GenRule[A]
case class RandomRule[A]() extends GenRule[A]
case class ConstRule[A](value: A) extends GenRule[A]

object GenRule {
  def gen[A, O](implicit mkGen: MkGen.Aux[A, O]): O = ???
}

trait MkGen[A] {
  type Out
}

object MkGen {
  type Aux[A, O] = GenRule[A] { type Out = O }
}
