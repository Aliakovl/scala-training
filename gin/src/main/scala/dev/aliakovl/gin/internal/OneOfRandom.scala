package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import dev.aliakovl.gin.internal.OneOfRandom._

import scala.util.{Random => ScalaRandom}

trait OneOfRandom {
  def oneOf2[A, B]: ApplyOneOf2[A, B] = new ApplyOneOf2[A, B]()

  def oneOf3[A, B, C]: ApplyOneOf3[A, B, C] = new ApplyOneOf3[A, B, C]()

  def oneOf4[A, B, C, D]: ApplyOneOf4[A, B, C, D] =
    new ApplyOneOf4[A, B, C, D]()

  def oneOf5[A, B, C, D, E]: ApplyOneOf5[A, B, C, D, E] =
    new ApplyOneOf5[A, B, C, D, E]()

  def oneOf6[A, B, C, D, E, F]: ApplyOneOf6[A, B, C, D, E, F] =
    new ApplyOneOf6[A, B, C, D, E, F]()

  def oneOf7[A, B, C, D, E, F, G]: ApplyOneOf7[A, B, C, D, E, F, G] =
    new ApplyOneOf7[A, B, C, D, E, F, G]()

  def oneOf8[A, B, C, D, E, F, G, H]: ApplyOneOf8[A, B, C, D, E, F, G, H] =
    new ApplyOneOf8[A, B, C, D, E, F, G, H]()

  def oneOf9[A, B, C, D, E, F, G, H, I]
      : ApplyOneOf9[A, B, C, D, E, F, G, H, I] =
    new ApplyOneOf9[A, B, C, D, E, F, G, H, I]()

  def oneOf10[A, B, C, D, E, F, G, H, I, J]
      : ApplyOneOf10[A, B, C, D, E, F, G, H, I, J] =
    new ApplyOneOf10[A, B, C, D, E, F, G, H, I, J]()

  def oneOf11[A, B, C, D, E, F, G, H, I, J, K]
      : ApplyOneOf11[A, B, C, D, E, F, G, H, I, J, K] =
    new ApplyOneOf11[A, B, C, D, E, F, G, H, I, J, K]()

  def oneOf12[A, B, C, D, E, F, G, H, I, J, K, L]
      : ApplyOneOf12[A, B, C, D, E, F, G, H, I, J, K, L] =
    new ApplyOneOf12[A, B, C, D, E, F, G, H, I, J, K, L]()

  def oneOf13[A, B, C, D, E, F, G, H, I, J, K, L, M]
      : ApplyOneOf13[A, B, C, D, E, F, G, H, I, J, K, L, M] =
    new ApplyOneOf13[A, B, C, D, E, F, G, H, I, J, K, L, M]()

  def oneOf14[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
      : ApplyOneOf14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] =
    new ApplyOneOf14[A, B, C, D, E, F, G, H, I, J, K, L, M, N]()

  def oneOf15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
      : ApplyOneOf15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] =
    new ApplyOneOf15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]()

  def oneOf16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
      : ApplyOneOf16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] =
    new ApplyOneOf16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]()

  def oneOf17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
      : ApplyOneOf17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] =
    new ApplyOneOf17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]()

  def oneOf18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
      : ApplyOneOf18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] =
    new ApplyOneOf18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]()

  def oneOf19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
      : ApplyOneOf19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] =
    new ApplyOneOf19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]()

  def oneOf20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
      : ApplyOneOf20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] =
    new ApplyOneOf20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]()

  def oneOf21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
      : ApplyOneOf21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] =
    new ApplyOneOf21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]()

  def oneOf22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
      : ApplyOneOf22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] =
    new ApplyOneOf22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]()
}

object OneOfRandom {
  private def choose[A](values: Random[A]*): Random[A] = Random {
    val index = ScalaRandom.nextInt(values.size)
    values(index).get()
  }

  final class ApplyOneOf2[A, B](private val dummy: Boolean = true)
      extends AnyVal {
    def make[TT, A1 >: A <: TT, B1 >: B <: TT](implicit
        ra: Random[A1],
        rb: Random[B1]
    ): Random[TT] = choose(ra.widen[TT], rb.widen[TT])
  }

  final class ApplyOneOf3[A, B, C](private val dummy: Boolean = true)
      extends AnyVal {
    def make[TT, A1 >: A <: TT, B1 >: B <: TT, C1 >: C <: TT](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1]
    ): Random[TT] = choose(ra.widen[TT], rb.widen[TT], rc.widen[TT])
  }

  final class ApplyOneOf4[A, B, C, D](private val dummy: Boolean = true)
      extends AnyVal {
    def make[TT, A1 >: A <: TT, B1 >: B <: TT, C1 >: C <: TT, D1 >: D <: TT](
        implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1]
    ): Random[TT] =
      choose(ra.widen[TT], rb.widen[TT], rc.widen[TT], rd.widen[TT])
  }
  final class ApplyOneOf5[A, B, C, D, E](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT]
    )
  }
  final class ApplyOneOf6[A, B, C, D, E, F](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT]
    )
  }
  final class ApplyOneOf7[A, B, C, D, E, F, G](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT]
    )
  }
  final class ApplyOneOf8[A, B, C, D, E, F, G, H](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT]
    )
  }
  final class ApplyOneOf9[A, B, C, D, E, F, G, H, I](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT]
    )
  }
  final class ApplyOneOf10[A, B, C, D, E, F, G, H, I, J](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT]
    )
  }
  final class ApplyOneOf11[A, B, C, D, E, F, G, H, I, J, K](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT]
    )
  }
  final class ApplyOneOf12[A, B, C, D, E, F, G, H, I, J, K, L](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT]
    )
  }
  final class ApplyOneOf13[A, B, C, D, E, F, G, H, I, J, K, L, M](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT]
    )
  }
  final class ApplyOneOf14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT]
    )
  }
  final class ApplyOneOf15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT]
    )
  }
  final class ApplyOneOf16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT,
        P1 >: P <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1],
        rp: Random[P1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT],
      rp.widen[TT]
    )
  }
  final class ApplyOneOf17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT,
        P1 >: P <: TT,
        Q1 >: Q <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1],
        rp: Random[P1],
        rq: Random[Q1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT],
      rp.widen[TT],
      rq.widen[TT]
    )
  }
  final class ApplyOneOf18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT,
        P1 >: P <: TT,
        Q1 >: Q <: TT,
        R1 >: R <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1],
        rp: Random[P1],
        rq: Random[Q1],
        rr: Random[R1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT],
      rp.widen[TT],
      rq.widen[TT],
      rr.widen[TT]
    )
  }
  final class ApplyOneOf19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT,
        P1 >: P <: TT,
        Q1 >: Q <: TT,
        R1 >: R <: TT,
        S1 >: S <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1],
        rp: Random[P1],
        rq: Random[Q1],
        rr: Random[R1],
        rs: Random[S1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT],
      rp.widen[TT],
      rq.widen[TT],
      rr.widen[TT],
      rs.widen[TT]
    )
  }
  final class ApplyOneOf20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT,
        P1 >: P <: TT,
        Q1 >: Q <: TT,
        R1 >: R <: TT,
        S1 >: S <: TT,
        T1 >: T <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1],
        rp: Random[P1],
        rq: Random[Q1],
        rr: Random[R1],
        rs: Random[S1],
        rt: Random[T1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT],
      rp.widen[TT],
      rq.widen[TT],
      rr.widen[TT],
      rs.widen[TT],
      rt.widen[TT]
    )
  }
  final class ApplyOneOf21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT,
        P1 >: P <: TT,
        Q1 >: Q <: TT,
        R1 >: R <: TT,
        S1 >: S <: TT,
        T1 >: T <: TT,
        U1 >: U <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1],
        rp: Random[P1],
        rq: Random[Q1],
        rr: Random[R1],
        rs: Random[S1],
        rt: Random[T1],
        ru: Random[U1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT],
      rp.widen[TT],
      rq.widen[TT],
      rr.widen[TT],
      rs.widen[TT],
      rt.widen[TT],
      ru.widen[TT]
    )
  }
  final class ApplyOneOf22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        TT,
        A1 >: A <: TT,
        B1 >: B <: TT,
        C1 >: C <: TT,
        D1 >: D <: TT,
        E1 >: E <: TT,
        F1 >: F <: TT,
        G1 >: G <: TT,
        H1 >: H <: TT,
        I1 >: I <: TT,
        J1 >: J <: TT,
        K1 >: K <: TT,
        L1 >: L <: TT,
        M1 >: M <: TT,
        N1 >: N <: TT,
        O1 >: O <: TT,
        P1 >: P <: TT,
        Q1 >: Q <: TT,
        R1 >: R <: TT,
        S1 >: S <: TT,
        T1 >: T <: TT,
        U1 >: U <: TT,
        V1 >: V <: TT
    ](implicit
        ra: Random[A1],
        rb: Random[B1],
        rc: Random[C1],
        rd: Random[D1],
        re: Random[E1],
        rf: Random[F1],
        rg: Random[G1],
        rh: Random[H1],
        ri: Random[I1],
        rj: Random[J1],
        rk: Random[K1],
        rl: Random[L1],
        rm: Random[M1],
        rn: Random[N1],
        ro: Random[O1],
        rp: Random[P1],
        rq: Random[Q1],
        rr: Random[R1],
        rs: Random[S1],
        rt: Random[T1],
        ru: Random[U1],
        rv: Random[V1]
    ): Random[TT] = choose(
      ra.widen[TT],
      rb.widen[TT],
      rc.widen[TT],
      rd.widen[TT],
      re.widen[TT],
      rf.widen[TT],
      rg.widen[TT],
      rh.widen[TT],
      ri.widen[TT],
      rj.widen[TT],
      rk.widen[TT],
      rl.widen[TT],
      rm.widen[TT],
      rn.widen[TT],
      ro.widen[TT],
      rp.widen[TT],
      rq.widen[TT],
      rr.widen[TT],
      rs.widen[TT],
      rt.widen[TT],
      ru.widen[TT],
      rv.widen[TT]
    )
  }
}
