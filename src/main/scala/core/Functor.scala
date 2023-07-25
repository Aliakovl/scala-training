package core

trait Functor[F[_]] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]
}

object Functor {
  def apply[F[_]](implicit inst: Functor[F]): Functor[F] = inst

  implicit class FunctorOps[A, F[_]](private val fa: F[A])(implicit
      inst: Functor[F]
  ) {
    def map[B](f: A => B): F[B] = inst.map(f)(fa)
  }
}
