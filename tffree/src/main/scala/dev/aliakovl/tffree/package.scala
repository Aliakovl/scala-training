package dev.aliakovl

import dev.aliakovl.core.Monad

package object tffree {
  case class EvalExpr[A](run: A)

  implicit val evalExprMonad: Monad[EvalExpr] = new Monad[EvalExpr] {
    override def flatMap[A, B](
        fa: EvalExpr[A]
    )(f: A => EvalExpr[B]): EvalExpr[B] = {
      EvalExpr(f(fa.run).run)
    }

    override def pure[A](a: A): EvalExpr[A] = EvalExpr(a)
  }

  case class ToString[A](run: String)

}
