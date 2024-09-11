package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import dev.aliakovl.gin.internal.OneOfRandom.OneOfTypePartiallyApplied

trait OneOfRandom {
  def oneOf[A](values: A*): Random[A] = Random {
    OneOfRandom.oneOfImpl(values)
  }

  def oneOf[W]: OneOfTypePartiallyApplied[W] = new OneOfTypePartiallyApplied[W]()

  def oneOfRandom[A](values: Random[A]*): Random[A] = OneOfRandom.oneOfRandomImpl(values: _*)
}

object OneOfRandom {
  private def oneOfImpl[A](values: Seq[A]): A = {
    val index = scala.util.Random.nextInt(values.size)
    values(index)
  }

  private def oneOfRandomImpl[A](values: Random[A]*): Random[A] = Random {
    oneOfImpl(values)()
  }

  final class OneOfTypePartiallyApplied[W](private val dummy: Boolean = true) extends AnyVal {
    def make[A <: W, B <: W](implicit ra: Random[A], rb: Random[B]): Random[W] = oneOfRandomImpl(ra, rb)
    def make[A <: W, B <: W, C <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C]): Random[W] = oneOfRandomImpl(ra, rb, rc)
    def make[A <: W, B <: W, C <: W, D <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O], rp: Random[P]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O], rp: Random[P], rq: Random[Q]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O], rp: Random[P], rq: Random[Q], rr: Random[R]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O], rp: Random[P], rq: Random[Q], rr: Random[R], rs: Random[S]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O], rp: Random[P], rq: Random[Q], rr: Random[R], rs: Random[S], rt: Random[T]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs, rt)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O], rp: Random[P], rq: Random[Q], rr: Random[R], rs: Random[S], rt: Random[T], ru: Random[U]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs, rt, ru)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W, V <: W](implicit ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D], re: Random[E], rf: Random[F], rg: Random[G], rh: Random[H], ri: Random[I], rj: Random[J], rk: Random[K], rl: Random[L], rm: Random[M], rn: Random[N], ro: Random[O], rp: Random[P], rq: Random[Q], rr: Random[R], rs: Random[S], rt: Random[T], ru: Random[U], rv: Random[V]): Random[W] = oneOfRandomImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs, rt, ru, rv)
  }
}
