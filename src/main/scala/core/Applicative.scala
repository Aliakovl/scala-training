package core

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  override def map[A, B](f: A => B)(fa: F[A]): F[B] = ap(pure(f))(fa)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map[A, B => (A, B)](a => b => (a, b))(fa))(fb)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    map(f.tupled)(product(fa, fb))
}

object Applicative {
  def apply[F[_]](implicit inst: Applicative[F]): Applicative[F] = inst

  implicit class ApplicativeOps[A, F[_]](private val fa: F[A])(implicit
      inst: Applicative[F]
  ) {
    def pure(a: A): F[A] = inst.pure(a)
    def ap[B, C](fb: F[B])(implicit ev: A <:< (B => C)): F[C] =
      inst.ap(fa.asInstanceOf[F[B => C]])(fb)
    def product[B](fb: F[B]): F[(A, B)] = inst.product(fa, fb)
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      ff match {
        case f :: restF =>
          fa match {
            case x :: xs => f(x) :: ap(ff)(xs)
            case Nil     => ap(restF)(fa)
          }
        case Nil => List.empty
      }
  }

  implicit val optionApplicative: Applicative[Option] =
    new Applicative[Option] {
      override def pure[A](a: A): Option[A] = Some(a)

      override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
        (ff, fa) match {
          case (Some(f), Some(a)) => Some(f(a))
          case _                  => None
        }
    }
}
