package dev.aliakovl.gin.macros.fp

package object data {
  type ValidatedC[+E, +A] = Validated[Chain[E], A]

  object ValidatedC {
    def invalid[E, A](e: E): ValidatedC[E, A] = Validated.invalid(Chain.one(e))
  }
}
