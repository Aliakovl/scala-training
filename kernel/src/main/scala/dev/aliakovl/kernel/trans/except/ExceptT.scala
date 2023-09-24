package dev.aliakovl.kernel.trans.except

opaque type ExceptT[M[+_], +E, +A] = M[Either[E, A]]

object ExceptT:
  export Inner.given
  export Inner.*

  inline def apply[M[+_], E, A](m: M[Either[E, A]]): ExceptT[M, E, A] = m

  extension [M[+_], E, A](exceptT: ExceptT[M, E, A])
    inline def runExceptT: M[Either[E, A]] = exceptT
