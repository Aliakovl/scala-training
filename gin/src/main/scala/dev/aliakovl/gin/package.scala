package dev.aliakovl

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions

package object gin {
  implicit final class GenWhen[A](private val value: A) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???

    @compileTimeOnly("arg can only be used inside specify")
    def arg[P](name: String): P = ???
  }

  implicit final class GenWhenF[F[_], A](private val value: F[A]) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def whenK[G[_] <: F[_]]: G[A] = ???
  }

  implicit def widen[A, B >: A](ra: Gen[A]): Gen[B] = ra.widen[B]
}
