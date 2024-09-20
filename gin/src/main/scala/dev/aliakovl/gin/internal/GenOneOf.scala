package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Gen
import dev.aliakovl.gin.internal.GenOneOf.OneOfTypePartiallyApplied

trait GenOneOf {
  def oneOf[A](values: A*): Gen[A] = Gen(GenOneOf.oneOfImpl(values))

  def oneOf[W]: OneOfTypePartiallyApplied[W] = new OneOfTypePartiallyApplied[W]()

  def oneOfGen[A](values: Gen[A]*): Gen[A] = GenOneOf.oneOfGenImpl(values: _*)
}

object GenOneOf {
  private def oneOfImpl[A](values: Seq[A]): A = {
    val index = scala.util.Random.nextInt(values.size)
    values(index)
  }

  private def oneOfGenImpl[A](values: Gen[A]*): Gen[A] = Gen {
    oneOfImpl(values)()
  }

  final class OneOfTypePartiallyApplied[W](private val dummy: Boolean = true) extends AnyVal {
    def make[A <: W, B <: W](implicit a: Gen[A], b: Gen[B]): Gen[W] = oneOfGenImpl(a, b)
    def make[A <: W, B <: W, C <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C]): Gen[W] = oneOfGenImpl(a, b, c)
    def make[A <: W, B <: W, C <: W, D <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D]): Gen[W] = oneOfGenImpl(a, b, c, d)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E]): Gen[W] = oneOfGenImpl(a, b, c, d, e)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W, V <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U], v: Gen[V]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  }
}
