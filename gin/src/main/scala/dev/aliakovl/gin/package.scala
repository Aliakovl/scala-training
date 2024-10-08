package dev.aliakovl

import dev.aliakovl.gin.internal.{Clarify1, Clarify2}

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions

package object gin {
  implicit final class GenWhen[A](private val value: A) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???

    @compileTimeOnly("arg can only be used inside specify")
    def arg[P](name: String): P = ???

    @compileTimeOnly("when1 can only be used inside specify")
    def when[F[_]](implicit ev: Clarify1[A, F]): ev.Out = ???

    @compileTimeOnly("when2 can only be used inside specify")
    def when[F[_, _]](implicit ev: Clarify2[A, F]): ev.Out = ???
  }

  implicit def widen[A, B >: A](ra: Gen[A]): Gen[B] = ra.widen[B]
}
