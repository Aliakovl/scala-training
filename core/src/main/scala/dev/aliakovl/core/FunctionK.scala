package dev.aliakovl.core

trait FunctionK[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
