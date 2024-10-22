package dev.aliakovl

import dev.aliakovl.gin.internal.{HasConstructor1, HasConstructor2}

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions

package object gin {
  implicit final class GenCustomOps[A](private val value: A) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???

    @compileTimeOnly("when can only be used inside specify")
    def when[F[_]](implicit ev: A HasConstructor1 F): ev.Out = ???

    @compileTimeOnly("when can only be used inside specify")
    def when[F[_, _]](implicit ev: A HasConstructor2 F): ev.Out = ???

    @compileTimeOnly("arg can only be used inside specify")
    def arg[P](name: String): P = ???
  }

  implicit def widen[A, B >: A](ra: Gen[A]): Gen[B] = ra.widen[B]
}
