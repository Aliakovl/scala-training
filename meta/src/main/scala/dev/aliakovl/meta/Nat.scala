package dev.aliakovl.meta

import scala.compiletime.erasedValue

sealed trait Nat
class _0 extends Nat
class Succ[A <: Nat] extends Nat

object Nat:
  transparent inline def toStringT[A <: Nat]: String = {
    inline erasedValue[A] match
      case _: _0      => "z"
      case _: Succ[n] => s"s(${toStringT[n]})"
  }

  transparent inline def toIntT[N <: Nat]: Int =
    inline erasedValue[N] match
      case _: _0      => 0
      case _: Succ[n] => toIntT[n] + 1

  type Sum[A <: Nat, B <: Nat] = A match
    case _0      => B
    case Succ[c] => Sum[c, Succ[B]]

  type Mult[A <: Nat, B <: Nat] = A match
    case _0      => _0
    case Succ[c] => Sum[B, Mult[c, B]]
