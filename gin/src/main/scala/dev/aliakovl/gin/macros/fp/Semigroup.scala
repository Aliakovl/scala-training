package dev.aliakovl.gin.macros.fp

trait Semigroup[A] {
  def combine(a: A, b: A): A
}
