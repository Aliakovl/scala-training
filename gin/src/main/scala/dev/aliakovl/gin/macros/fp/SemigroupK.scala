package dev.aliakovl.gin.macros.fp

trait SemigroupK[F[_]] {
  def combineK[A](a: F[A], b: F[A]): F[A]
}
