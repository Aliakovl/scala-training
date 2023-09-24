package dev.aliakovl.kernel

trait Monad[M[_]] extends Applicative[M]:
  extension [A, MM[T] <: M[T]](ma: MM[A]) def flatMap[B](f: A => M[B]): M[B]

  extension [A, B, MM[T] <: M[T]](ff: MM[A => B])
    def ap(fa: M[A]): M[B] =
      ff.flatMap { f => fa.map(f) }

  extension [A, MM[T] <: M[T]](ma: MM[A])
    override def map[B](f: A => B): M[B] =
      ma.flatMap { a => pure(f(a)) }
