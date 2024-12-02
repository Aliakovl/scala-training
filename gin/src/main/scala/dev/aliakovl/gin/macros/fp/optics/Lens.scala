package dev.aliakovl.gin.macros.fp.optics

private[macros] trait Lens[T, S] {
  def get(t: T): S
  def set(t: T, s: S): T
  def modify(t: T, f: S => S): T = set(t, f(get(t)))
}

private[macros] object Lens {
  def apply[T, S](_get: T => S)(_set: T => S => T): Lens[T, S] =
    new Lens[T, S] {
      override def get(t: T): S = _get(t)
      override def set(t: T, s: S): T = _set(t)(s)
    }

  final class LensOps[T](private val value: T) extends AnyVal {
    def modify[S](f: S => S)(implicit lens: Lens[T, S]): T =
      lens.modify(value, f)
    def set[S](s: S)(implicit lens: Lens[T, S]): T = lens.set(value, s)
  }
}
