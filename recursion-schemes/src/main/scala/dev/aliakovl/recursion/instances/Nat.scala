package dev.aliakovl.recursion.instances

import dev.aliakovl.kernel.Functor
import dev.aliakovl.recursion.fix.*
import dev.aliakovl.recursion.instances.*
import dev.aliakovl.recursion.schemes.Catamorphism.*

sealed trait Natural
object Zero extends Natural
case class Succ(nat: Natural) extends Natural

sealed trait N[+R]
object Z extends N[Nothing]
case class S[+R](r: R) extends N[R]

object N:
  given Functor[N] with
    extension[A, FF[T] <: N[T]] (fa: FF[A])
      def map[B](f: A => B): N[B] = fa match
        case S(x) => S(f(x))
        case Z => Z

type Nat = Fix[N]

object NatMain:

  val z: N[Nat] = Z
  val _z: Nat = In(Z)
  val sz: N[Nat] = S(In(Z))
  val _sz: Nat = In(S(In(Z)))
  val ssz: N[Nat] = S(In(S(In(Z))))
  val _ssz: Nat = In(S(In(S(In(Z)))))

  val two: Int = _ssz.cata[Int] {
    case S(x) => x + 1
    case Z => 0
  }

  def main(args: Array[String]): Unit = println(two)
