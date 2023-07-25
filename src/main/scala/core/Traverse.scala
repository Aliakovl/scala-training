package core

trait Traverse[T[_]] extends Functor[T] with Foldable[T] {
  def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]] =
    sequenceA(map(f)(ta))
  def sequenceA[F[_]: Applicative, A](tfa: T[F[A]]): F[T[A]] =
    traverse(tfa)(identity)
}

object Traverse {
  def apply[T[_]](implicit inst: Traverse[T]): Traverse[T] = inst

  implicit class TraversableOpt[T[_], A](private val ta: T[A])(implicit
      inst: Traverse[T]
  ) {
    def traverse[F[_]: Applicative, B](f: A => F[B]): F[T[B]] =
      inst.traverse(ta)(f)
    def sequenceA[F[_]: Applicative, B](implicit ev: A <:< F[B]): F[T[B]] =
      inst.sequenceA[F, B](ta.asInstanceOf[T[F[B]]])
  }

  implicit val listTraverse: Traverse[List] = new Traverse[List] {
    override def foldRight[A, B](ta: List[A], ini: B)(f: (A, B) => B): B =
      ta.foldRight(ini)(f)

    override def foldLeft[A, B](ta: List[A], ini: B)(f: (B, A) => B): B =
      ta.foldLeft(ini)(f)

    override def map[A, B](f: A => B)(fa: List[A]): List[B] = fa.map(f)

    override def traverse[F[_]: Applicative, A, B](
        ta: List[A]
    )(f: A => F[B]): F[List[B]] =
      foldRight(ta, Applicative[F].pure(List.empty[B])) { (a, acc) =>
        Applicative[F].map2(acc, f(a)) { (xs, x) => x :: xs }
      }
  }
}
