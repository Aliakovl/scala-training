package dev.aliakovl.kernel

trait Monad[M[_]] extends Functor[M]:
  def pure[A](a: A): M[A]
  extension[A] (ma: M[A]) def flatMap[B](f: A => M[B]): M[B]
  extension[A] (ma: M[A]) override def map[B](f: A => B): M[B] =
    ma.flatMap { a => pure(f(a)) }

object Monad:
  def apply[M[_]](using m: Monad[M]): Monad[M] = m

  given Monad[Option] with
    def pure[A](a: A): Option[A] = Some(a)
    extension[A] (ma: Option[A])
      def flatMap[B](f: A => Option[B]): Option[B] = ma match
        case Some(x) => f(x)
        case None => None
