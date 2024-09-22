package dev.aliakovl.gin.internal

import cats.Monad
import dev.aliakovl.gin.Gen


abstract private[gin] class GenLowPriority {
  implicit val catsMonadForGen: Monad[Gen] = new Monad[Gen] {
    override def pure[A](x: A): Gen[A] = Gen(x)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)

    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)

    override def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] = Gen { r =>
      f(a)(r) match {
        case Left(value)  => tailRecM(value)(f)(r)
        case Right(value) => value
      }
    }
  }
}
