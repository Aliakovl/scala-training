package dev.aliakovl.gin.internal

trait Clarify1[A, F[_]] {
  type Out
}

object Clarify1 {
  type Aux[A, F[_], Out0] = Clarify1[A, F] { type Out = Out0 }

  def apply[A, F[_], Out0]: Aux[A, F, Out0] = new Clarify1[A, F] {
    override type Out = Out0
  }

  implicit def clarify1[F[TT] <: G[TT], G[_], T]: Aux[G[T], F, F[T]] = {
    Clarify1[G[T], F, F[T]]
  }

  implicit def clarify21[F[LL] <: G[LL, R], G[_, _],  T, R]: Aux[G[T, R], F, F[T]] = {
    Clarify1[G[T, R], F, F[T]]
  }
}

trait Clarify2[A, F[_, _]] {
  type Out
}

object Clarify2 {
  def apply[A, F[_, _], Out0]: Clarify2.Aux[A, F, Out0] = new Clarify2[A, F] {
    override type Out = Out0
  }

  type Aux[A, F[_, _], Out0] = Clarify2[A, F] { type Out = Out0 }

  implicit def clarify2[F[LL, RR] <: G[LL, RR], G[_, _], L, R]: Aux[G[L, R], F, F[L, R]] = {
    Clarify2[G[L, R], F, F[L, R]]
  }

  implicit def clarify22[F[LL, RR] <: G[RR, LL], G[_, _], L, R]: Aux[G[R, L], F, F[L, R]] = {
    Clarify2[G[R, L], F, F[L, R]]
  }
}
