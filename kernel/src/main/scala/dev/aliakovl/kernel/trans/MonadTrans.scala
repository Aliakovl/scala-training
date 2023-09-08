package dev.aliakovl.kernel.trans

import dev.aliakovl.kernel.Monad

trait MonadTrans[T[_[+_], _]]:
  def lift[M[+_] : Monad, A](ma: M[A]): T[M, A]

object MonadTrans:
  inline def apply[T[_[+_], _]](using t: MonadTrans[T]): MonadTrans[T] = t
