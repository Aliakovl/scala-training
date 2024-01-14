package dev.aliakovl.monad.data

case class Op[A, B](run: B => A)

type <=[A, B] = Op[A, B]
