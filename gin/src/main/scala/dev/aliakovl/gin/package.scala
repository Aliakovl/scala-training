package dev.aliakovl

import scala.annotation.compileTimeOnly
import scala.collection.Factory
import scala.language.implicitConversions

package object gin {
  implicit final class GenWhen[A](private val value: A) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???
  }

  implicit final class GenOps[A](private val gen: Gen[A]) extends AnyVal {
    def many[C[E] <: IterableOnce[E]](size: Int)(implicit
        f: Factory[A, C[A]]
    ): Gen[C[A]] = Gen.many[C](size).apply(gen)
  }

  implicit def widen[A, B >: A](ra: Gen[A]): Gen[B] = ra.widen[B]
}
