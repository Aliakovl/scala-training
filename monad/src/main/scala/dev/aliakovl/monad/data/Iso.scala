package dev.aliakovl.monad.data

case class Iso[A, B](f: A => B, g: B => A)

type ~[A, B] = Iso[A, B]
