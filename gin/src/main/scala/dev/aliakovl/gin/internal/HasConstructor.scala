package dev.aliakovl.gin
package internal

import scala.annotation.implicitNotFound

@implicitNotFound("\nType ${A} does not have constructor ${F}")
trait HasConstructor1[A, F[_]] {
  type Out
}

object HasConstructor1 {
  type Aux[A, F[_], Out0] = HasConstructor1[A, F] { type Out = Out0 }

  def apply[A, F[_], Out0]: Aux[A, F, Out0] = new HasConstructor1[A, F] {
    override type Out = Out0
  }

  implicit def impl[F[TT] <: G[TT], G[_], T]: Aux[G[T], F, F[T]] = {
    HasConstructor1[G[T], F, F[T]]
  }

  implicit def impl2[F[LL] <: G[LL, R], G[_, _],  T, R]: Aux[G[T, R], F, F[T]] = {
    HasConstructor1[G[T, R], F, F[T]]
  }
}

@implicitNotFound("\nType ${A} does not have constructor ${F}")
trait HasConstructor2[A, F[_, _]] {
  type Out
}

object HasConstructor2 {
  type Aux[A, F[_, _], Out0] = HasConstructor2[A, F] { type Out = Out0 }

  def apply[A, F[_, _], Out0]: Aux[A, F, Out0] = new HasConstructor2[A, F] {
    override type Out = Out0
  }

  implicit def impl[F[LL, RR] <: G[LL, RR], G[_, _], L, R]: Aux[G[L, R], F, F[L, R]] = {
    HasConstructor2[G[L, R], F, F[L, R]]
  }

  implicit def impl2[F[LL, RR] <: G[RR, LL], G[_, _], L, R]: Aux[G[R, L], F, F[L, R]] = {
    HasConstructor2[G[R, L], F, F[L, R]]
  }
}
