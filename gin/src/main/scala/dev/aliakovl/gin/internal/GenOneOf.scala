package dev.aliakovl.gin
package internal

import dev.aliakovl.gin.internal.GenOneOf.OneOfTypePartiallyApplied

import scala.collection.immutable.TreeMap

trait GenOneOf {
  def oneOf[A](values: A*): Gen[A] = Gen(random => GenOneOf.oneOfImpl(random)(values))

  def one[W]: OneOfTypePartiallyApplied[W] = new OneOfTypePartiallyApplied[W]()

  def oneOfGen[A](values: Gen[A]*): Gen[A] = GenOneOf.oneOfGenImpl(values: _*)

  def frequencyGen[A](cases: (Int, Gen[A])*): Gen[A] = {
    if (cases.isEmpty) {
      throw new IllegalArgumentException("cases must not be empty")
    }

    cases.foreach { case (x, _) =>
      if (x <= 0) {
        throw new IllegalArgumentException("weights mast be positive")
      }
    }

    val (max, backets) = {
      var max = 0L
      val builder = TreeMap.newBuilder[Long, Gen[A]]

      cases.foreach { case (weight, value) =>
        max += weight
        builder += (max -> value)
      }

      (max, builder.result())
    }

    Gen.between(1L, max + 1).flatMap { next =>
      backets.rangeFrom(next).head._2
    }
  }

  def frequency[A](cases: (Int, A)*): Gen[A] = frequencyGen[A](cases.map { case (weight, value) =>
    weight -> Gen.const(value)
  }: _*)
}

object GenOneOf {
  private def oneOfImpl[A](r: Random)(values: Seq[A]): A = {
    val index = r.nextInt(values.size)
    values(index)
  }

  private def oneOfGenImpl[A](values: Gen[A]*): Gen[A] = Gen { random =>
    oneOfImpl(random)(values)(random)
  }

  final class OneOfTypePartiallyApplied[W](private val dummy: Boolean = true) extends AnyVal {
    def of[A <: W, B <: W](implicit a: Gen[A], b: Gen[B]): Gen[W] = oneOfGenImpl(a, b)
    def of[A <: W, B <: W, C <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C]): Gen[W] = oneOfGenImpl(a, b, c)
    def of[A <: W, B <: W, C <: W, D <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D]): Gen[W] = oneOfGenImpl(a, b, c, d)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E]): Gen[W] = oneOfGenImpl(a, b, c, d, e)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    def of[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W, V <: W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U], v: Gen[V]): Gen[W] = oneOfGenImpl(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  }
}
