package dev.aliakovl.recursion.schemes

import dev.aliakovl.kernel.Functor
import dev.aliakovl.recursion.fix.*

object Anamorphism:
  def ana[F[_]: Functor, A](coalg: A => F[A])(a: A): Fix[F] =
    In(coalg(a).map(ana(coalg)))

  extension[A] (a: A)
    def ana[F[_]: Functor](coalg: A => F[A]): Fix[F] = Anamorphism.ana(coalg)(a)
