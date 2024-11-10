package dev.aliakovl.gin.macros.fp

package object data {
  type ValidatedC[+E, +A] = Validated[Chain[E], A]
}
