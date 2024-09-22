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
    ): Gen[C[A]] = Gen( r =>
      factory.fromSpecific(Iterable.fill(size)(gen(r)))
    )
  }

  implicit final class GenTuple2Ops[K, V](private val gen: Gen[(K, V)]) extends AnyVal {
    def makeMap(size: Int): Gen[Map[K, V]] = Gen( r =>
      Map.fromSpecific(Iterable.fill(size)(gen(r)))
    )
  }

  implicit def widen[A, B >: A](ra: Gen[A]): Gen[B] = ra.widen[B]
}
