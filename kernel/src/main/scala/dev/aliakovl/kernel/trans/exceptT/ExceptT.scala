package dev.aliakovl.kernel.trans.exceptT

opaque type ExceptT[M[_], E, A] = M[Either[E, A]]

object ExceptT:
  export Inner.given
  export Inner.*

  def apply[M[_], E, A](m: M[Either[E, A]]): ExceptT[M, E, A] = m

  extension[M[_], E, A] (e: ExceptT[M, E, A])
    def run: M[Either[E, A]] = e
