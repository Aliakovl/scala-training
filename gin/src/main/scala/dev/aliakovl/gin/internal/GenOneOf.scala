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
    def make[A <: W, B <: W](implicit ra: Gen[A], rb: Gen[B]): Gen[W] = oneOfGenImpl(ra, rb)
    def make[A <: W, B <: W, C <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C]): Gen[W] = oneOfGenImpl(ra, rb, rc)
    def make[A <: W, B <: W, C <: W, D <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O], rp: Gen[P]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O], rp: Gen[P], rq: Gen[Q]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O], rp: Gen[P], rq: Gen[Q], rr: Gen[R]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O], rp: Gen[P], rq: Gen[Q], rr: Gen[R], rs: Gen[S]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O], rp: Gen[P], rq: Gen[Q], rr: Gen[R], rs: Gen[S], rt: Gen[T]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs, rt)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O], rp: Gen[P], rq: Gen[Q], rr: Gen[R], rs: Gen[S], rt: Gen[T], ru: Gen[U]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs, rt, ru)
    def make[A <: W, B <: W, C <: W, D <: W, E <: W, F <: W, G <: W, H <: W, I <: W, J <: W, K <: W, L <: W, M <: W, N <: W, O <: W, P <: W, Q <: W, R <: W, S <: W, T <: W, U <: W, V <: W](implicit ra: Gen[A], rb: Gen[B], rc: Gen[C], rd: Gen[D], re: Gen[E], rf: Gen[F], rg: Gen[G], rh: Gen[H], ri: Gen[I], rj: Gen[J], rk: Gen[K], rl: Gen[L], rm: Gen[M], rn: Gen[N], ro: Gen[O], rp: Gen[P], rq: Gen[Q], rr: Gen[R], rs: Gen[S], rt: Gen[T], ru: Gen[U], rv: Gen[V]): Gen[W] = oneOfGenImpl(ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp, rq, rr, rs, rt, ru, rv)
  }
}
