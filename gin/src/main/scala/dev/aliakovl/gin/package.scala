package dev.aliakovl

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions

package object gin {
  implicit final class GenWhen[A](private val value: A) extends AnyVal {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???
  }

  implicit def constRandom[A](value: => A): Random[A] = Random.const(value)
}
