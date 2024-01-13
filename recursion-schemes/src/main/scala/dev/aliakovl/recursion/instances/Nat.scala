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
    extension [A, FF[T] <: N[T]](fa: FF[A])
      def map[B](f: A => B): N[B] = fa match
        case S(x) => S(f(x))
        case Z    => Z

type Z = Z.type
type Nat = Fix[N]

object NatMain:
  val rec: S[S[S[S[S[Z]]]]] = S(S(S(S(S(Z)))))

  val _z: N[Nat] = Z
  val z: Nat = In(Z)
  val _sz: N[Nat] = S(In(Z))
  val sz: Nat = In(S(In(Z)))
  val _ssz: N[Nat] = S(In(S(In(Z))))
  val ssz: Nat = In(S(In(S(In(Z)))))

  val two: Int = ssz.cata[Int] {
    case S(x) => x + 1
    case Z    => 0
  }

  def main(args: Array[String]): Unit = println(two)
