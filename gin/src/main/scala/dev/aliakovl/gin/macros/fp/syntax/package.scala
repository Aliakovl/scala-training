package dev.aliakovl.gin.macros.fp

import scala.language.implicitConversions

package object syntax {
  implicit def traverseSyntax[T[_], A](ta: T[A]): Traverse.TraverseOps[T, A] =
    new Traverse.TraverseOps(ta)

  implicit def sequenceSyntax[T[_], F[_], A](tfa: T[F[A]]): Traverse.SequenceOps[T, F, A] =
    new Traverse.SequenceOps(tfa)
}
