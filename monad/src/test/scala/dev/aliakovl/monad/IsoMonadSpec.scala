package dev.aliakovl.monad

import dev.aliakovl.monad.data.{Group, Id, Iso, Mul, Sum, ~}
import dev.aliakovl.monad.IsoMonad.given
import dev.aliakovl.monad.data.Group.given
import dev.aliakovl.monad.data.Group.*
import dev.aliakovl.monad.Monad.>=>

import scala.math.{exp, log}

object IsoMonadSpec:
  val iso: Double ~ Id[Double] = Iso[Double, Id[Double]](
    f = a => Id(exp(a)),
    g = a => log(a.value)
  )

  val result: Double ~ Id[Double] = iso >=> iso >=> iso

  val f: Id[Double] = result.f(1)
  val g: Double = result.g(f)

  val isoSM: Sum[Double] ~ Mul[Double] = Iso(
    f = a => Mul(exp(a.value)),
    g = a => Sum(log(a.value))
  )

  val mulGroup: Group[Mul[Double]] = summon[Group[Sum[Double]]].modify[Mul[Double]](isoSM)

  val isoSMF: Sum[Double] ~ Id[Mul[Double]] = Iso(
    f = a => Id(Mul(exp(a.value))),
    g = a => Sum(log(a.value.value))
  )

  val _isoSMF: Sum[Double] ~ Id[Mul[Double]] = IsoMonad[Id].pure >=> isoSMF >=> IsoMonad[Id].pure

  val harm: Sum[Double] = List[Double](1, 2, 5, 4, 3)
    .view
    .map(Mul.apply)
    .map(_.inverse)
    .map(m => Sum(m.value))
    .foldRight(Group[Sum[Double]].identity)(Group[Sum[Double]].combine)

  def main(args: Array[String]): Unit = {
    println(f.value)
    println(g)
    println(s"${mulGroup.combine(Mul(4), Mul(5))}, ${summon[Group[Mul[Double]]].combine(Mul(4), Mul(5))}")
    println(s"${mulGroup.identity}, ${summon[Group[Mul[Double]]].identity}")
    println(s"${mulGroup.inverse(Mul(5))}, ${summon[Group[Mul[Double]]].inverse(Mul(5))}")
    println(harm)
  }
