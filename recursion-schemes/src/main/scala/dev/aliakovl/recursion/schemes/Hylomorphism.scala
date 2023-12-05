package dev.aliakovl.recursion.schemes

import dev.aliakovl.kernel.Functor

object Hylomorphism:
  import Catamorphism.cata
  import Anamorphism.ana

  def hylo[F[_]: Functor, A, B](alg: F[A] => A)(coalg: B => F[B])(b: B): A =
    cata(alg).compose(ana(coalg))(b)
