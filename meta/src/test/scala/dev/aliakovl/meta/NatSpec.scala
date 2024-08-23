package dev.aliakovl.meta

import dev.aliakovl.meta.Nat.*

object NatSpec:
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]

  implicitly[Sum[_2, _3] =:= _5]
  implicitly[Mult[_2, _2] =:= _4]
  implicitly[Mult[_5, _0] =:= _0]

  type _8 = Sum[_3, _5]
  type _15 = Mult[_3, _5]

  def main(args: Array[String]): Unit = {
    println(s"${toStringT[_8]} -> ${toIntT[_8]}")
    println(s"${toStringT[_15]} -> ${toIntT[_15]}")
    println(s"${toStringT[Sum[_8, _15]]} -> ${toIntT[Sum[_8, _15]]}")
  }
