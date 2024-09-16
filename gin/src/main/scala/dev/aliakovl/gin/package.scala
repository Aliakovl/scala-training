package dev.aliakovl

import scala.annotation.compileTimeOnly
import scala.collection.Factory
import scala.language.implicitConversions

package object gin {
  implicit final class GenWhen[A](private val value: A) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???
  }

  implicit final class RandomMany[A](private val random: Random[A]) extends AnyVal {
    def many[C[E] <: IterableOnce[E]](size: Int)(implicit
        f: Factory[A, C[A]]
    ): Random[C[A]] = Random.many[C](size).apply(random)
  }

  implicit def widen[A, B >: A](ra: Random[A]): Random[B] = ra.widen[B]
}
