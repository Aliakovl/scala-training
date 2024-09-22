package dev.aliakovl

import scala.annotation.compileTimeOnly
import scala.collection.Factory
import scala.collection.MapFactory.toFactory
import scala.language.implicitConversions

package object gin {
  implicit final class GenWhen[A](private val value: A) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???

    @compileTimeOnly("arg can only be used inside specify")
    def arg[P](name: String): P = ???
  }

  implicit final class GenOps[A](private val gen: Gen[A]) extends AnyVal {
    def many[C[E] <: IterableOnce[E]](size: Int)(implicit
        factory: Factory[A, C[A]]
    ): Gen[C[A]] = Gen(
      factory.fromSpecific(Iterable.fill(size)(gen.apply()))
    )

    def makeMap[K, V](size: Int)(implicit ev: A <:< (K, V)): Gen[Map[K, V]] = Gen(
      Map.fromSpecific(Iterable.fill(size)(gen.apply()))
    )
  }

  implicit def widen[A, B >: A](ra: Gen[A]): Gen[B] = ra.widen[B]
}
