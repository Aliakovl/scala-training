package dev.aliakovl.gin
package internal

import cats.Monad

abstract private[gin] class GenLowPriority {
  implicit val catsMonadForGen: Monad[Gen] = new Monad[Gen] {
    override def pure[A](x: A): Gen[A] = Gen.const(x)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)

    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)

    override def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] = Gen { random =>
      f(a)(random) match {
        case Left(value)  => tailRecM(value)(f)(random)
        case Right(value) => value
      }
    }
  }
}
