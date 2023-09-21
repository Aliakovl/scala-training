package dev.aliakovl.derive

enum Lst[+T] derives Eq:
  case Cns(t: T, ts: Lst[T])
  case Nl

extension [T](t: T) def ::(ts: Lst[T]): Lst[T] = Lst.Cns(t, ts)

@main def test(): Unit =
  import Lst.*
  val eqoi = summon[Eq[Lst[Int]]]
  assert(eqoi.eqv(23 :: 47 :: Nl, 23 :: 47 :: Nl))
  assert(!eqoi.eqv(23 :: Nl, 7 :: Nl))
  assert(!eqoi.eqv(23 :: Nl, Nl))
