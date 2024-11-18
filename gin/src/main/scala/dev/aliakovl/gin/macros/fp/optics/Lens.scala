package dev.aliakovl.gin.macros.fp.optics

private[macros] trait Lens[T, S] {
  def get(t: T): S
  def set(t: T, s: S): T
  def modify(t: T, f: S => S): T = set(t, f(get(t)))
}

private[macros] object Lens {
  implicit def lensForTuple2_1[F, S]: Lens[(F, S), F] = new Lens[(F, S), F] {
    override def get(t: (F, S)): F = t._1
    override def set(t: (F, S), s: F): (F, S) = t.copy(_1 = s)
  }

  implicit def lensForTuple2_2[F, S]: Lens[(F, S), S] = new Lens[(F, S), S] {
    override def get(t: (F, S)): S = t._2
    override def set(t: (F, S), s: S): (F, S) = t.copy(_2 = s)
  }

  final class LensOps[T](private val value: T) extends AnyVal {
    def modify[S](f: S => S)(implicit lens: Lens[T, S]): T = lens.modify(value, f)
    def set[S](s: S)(implicit lens: Lens[T, S]): T = lens.set(value, s)
  }
}
