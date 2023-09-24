package dev.aliakovl.kernel.trans.reader

opaque type ReaderT[+M[+_], -R, +A] = R => M[A]

object ReaderT:
  export Inner.given
  export Inner.*

  inline def apply[M[+_], R, A](arrow: R => M[A]): ReaderT[M, R, A] = arrow

  extension [M[+_], R, A](readerT: ReaderT[M, R, A])
    inline def runReaderT: R => M[A] = readerT
