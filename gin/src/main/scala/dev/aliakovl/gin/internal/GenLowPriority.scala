package dev.aliakovl.gin
package internal

import cats.Monad

import scala.annotation.tailrec

trait GenLowPriority {
  implicit val catsMonadForGen: Monad[Gen] = new Monad[Gen] {
    override def pure[A](x: A): Gen[A] = Gen.const(x)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)

    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)

    override def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] =
      Gen { random =>
        @tailrec
        def step(thisA: A): B = f(thisA)(random) match {
          case Right(b)    => b
          case Left(nextA) => step(nextA)
        }
        step(a)
      }
  }
}
