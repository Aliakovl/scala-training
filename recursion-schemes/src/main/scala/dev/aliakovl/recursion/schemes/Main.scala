package dev.aliakovl.recursion.schemes

object Main {
  def main(args: Array[String]): Unit = {}
}

//sealed trait Natural
//object Zero extends Natural
//case class Succ(nat: Natural) extends Natural

sealed trait NatF[R]
object ZeroF extends NatF[Nothing]
case class SuccF[R](r: R) extends NatF[R]

sealed trait Fix[F[_]]
case class In[F[_]](out: F[Fix[F]]) extends Fix[F]

type Natural = Fix[NatF]
