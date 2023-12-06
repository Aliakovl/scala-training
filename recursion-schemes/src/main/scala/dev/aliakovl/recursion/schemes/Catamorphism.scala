package dev.aliakovl.recursion.schemes

import dev.aliakovl.kernel.Functor
import dev.aliakovl.recursion.fix.{Fix, In}

object Catamorphism:
  def copy[F[_] : Functor](fix: Fix[F]): Fix[F] = fix match
    case In(x) => In(x.map(copy))

  def cata[F[_]: Functor, A](alg: F[A] => A)(fix: Fix[F]): A = fix match
    case In(x) => alg(x.map(cata(alg)))

  extension[F[_]: Functor] (fix: Fix[F])
    def cata[A](alg: F[A] => A): A = Catamorphism.cata(alg)(fix)
