package dev.aliakovl

import scala.annotation.compileTimeOnly

package object gin {
  implicit final class GenWhen[A](private val value: A) {
    @compileTimeOnly("when can only be used inside specify")
    def when[B <: A]: B = ???
  }
}
