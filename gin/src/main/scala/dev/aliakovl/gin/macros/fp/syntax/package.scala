package dev.aliakovl.gin.macros.fp

import dev.aliakovl.gin.macros.fp.optics.Lens

import scala.language.implicitConversions

package object syntax {
  implicit def traverseSyntax[T[_], A](ta: T[A]): Traverse.TraverseOps[T, A] =
    new Traverse.TraverseOps(ta)

  implicit def sequenceSyntax[T[_], F[_], A](tfa: T[F[A]]): Traverse.SequenceOps[T, F, A] =
    new Traverse.SequenceOps(tfa)

  implicit def lensSyntax[T](value: T): Lens.LensOps[T] = new Lens.LensOps[T](value)
}
