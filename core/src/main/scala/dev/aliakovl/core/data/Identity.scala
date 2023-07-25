package dev.aliakovl.core.data

import dev.aliakovl.core._

case class Identity[A](value: A) extends AnyVal

object Identity {
  implicit val identityFunctor: Functor[Identity] = new Functor[Identity] {
    override def map[A, B](f: A => B)(fa: Identity[A]): Identity[B] =
      Identity(f(fa.value))
  }

  implicit val identityApplicative: Applicative[Identity] = new Applicative[Identity] {
    override def pure[A](a: A): Identity[A] = Identity(a)

    override def ap[A, B](ff: Identity[A => B])(fa: Identity[A]): Identity[B] = Identity(ff.value(fa.value))
  }

}
