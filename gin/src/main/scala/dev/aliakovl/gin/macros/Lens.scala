package dev.aliakovl.gin.macros

trait Lens[T, S] {
  def get(t: T): S
  def set(t: T, s: S): T
}

object Lens {
  implicit def tupleLens[F, S]: Lens[(F, S), F] = new Lens[(F, S), F] {
    override def get(t: (F, S)): F = t._1
    override def set(t: (F, S), s: F): (F, S) = t.copy(_1 = s)
  }
}
