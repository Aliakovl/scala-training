package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random
import dev.aliakovl.gin.internal.OneOfRandom._

import scala.util.{Random => ScalaRandom}

trait OneOfRandom {
  def oneOf[A](values: A*): Random[A] = OneOfRandom.oneOfRandomImpl(values.map(a => Random(a)): _*)

  def oneOfRandom[A](values: Random[A]*): Random[A] = OneOfRandom.oneOfRandomImpl(values: _*)

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
  private def oneOfRandomImpl[A](values: Random[A]*): Random[A] = Random {
    val index = ScalaRandom.nextInt(values.size)
    values(index).apply()
  }

  final class ApplyOneOf2[A, B](private val dummy: Boolean = true)
      extends AnyVal {

    def apply[W, AA >: A <: W, BB >: B <: W](
        ra: Random[AA],
        rb: Random[BB]
    ): Random[W] = oneOfRandomImpl(ra.widen[W], rb.widen[W])

    def make[W, AA >: A <: W, BB >: B <: W](implicit
        ra: Random[AA],
        rb: Random[BB]
    ): Random[W] = oneOfRandomImpl(ra.widen[W], rb.widen[W])
  }

  final class ApplyOneOf3[A, B, C](private val dummy: Boolean = true)
      extends AnyVal {
    def make[W, AA >: A <: W, BB >: B <: W, CC >: C <: W](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC]
    ): Random[W] = oneOfRandomImpl(ra.widen[W], rb.widen[W], rc.widen[W])
  }

  final class ApplyOneOf4[A, B, C, D](private val dummy: Boolean = true)
      extends AnyVal {
    def make[W, AA >: A <: W, BB >: B <: W, CC >: C <: W, DD >: D <: W](
        implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD]
    ): Random[W] =
      oneOfRandomImpl(ra.widen[W], rb.widen[W], rc.widen[W], rd.widen[W])
  }
  final class ApplyOneOf5[A, B, C, D, E](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W]
    )
  }
  final class ApplyOneOf6[A, B, C, D, E, F](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W]
    )
  }
  final class ApplyOneOf7[A, B, C, D, E, F, G](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W]
    )
  }
  final class ApplyOneOf8[A, B, C, D, E, F, G, H](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W]
    )
  }
  final class ApplyOneOf9[A, B, C, D, E, F, G, H, I](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W]
    )
  }
  final class ApplyOneOf10[A, B, C, D, E, F, G, H, I, J](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W]
    )
  }
  final class ApplyOneOf11[A, B, C, D, E, F, G, H, I, J, K](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W]
    )
  }
  final class ApplyOneOf12[A, B, C, D, E, F, G, H, I, J, K, L](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W]
    )
  }
  final class ApplyOneOf13[A, B, C, D, E, F, G, H, I, J, K, L, M](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W]
    )
  }
  final class ApplyOneOf14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W]
    )
  }
  final class ApplyOneOf15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W]
    )
  }
  final class ApplyOneOf16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W,
        PP >: P <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO],
        rp: Random[PP]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W],
      rp.widen[W]
    )
  }
  final class ApplyOneOf17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
      private val dummy: Boolean = true
  ) extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W,
        PP >: P <: W,
        QQ >: Q <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO],
        rp: Random[PP],
        rq: Random[QQ]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W],
      rp.widen[W],
      rq.widen[W]
    )
  }
  final class ApplyOneOf18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W,
        PP >: P <: W,
        QQ >: Q <: W,
        RR >: R <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO],
        rp: Random[PP],
        rq: Random[QQ],
        rr: Random[RR]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W],
      rp.widen[W],
      rq.widen[W],
      rr.widen[W]
    )
  }
  final class ApplyOneOf19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W,
        PP >: P <: W,
        QQ >: Q <: W,
        RR >: R <: W,
        SS >: S <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO],
        rp: Random[PP],
        rq: Random[QQ],
        rr: Random[RR],
        rs: Random[SS]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W],
      rp.widen[W],
      rq.widen[W],
      rr.widen[W],
      rs.widen[W]
    )
  }
  final class ApplyOneOf20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W,
        PP >: P <: W,
        QQ >: Q <: W,
        RR >: R <: W,
        SS >: S <: W,
        TT >: T <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO],
        rp: Random[PP],
        rq: Random[QQ],
        rr: Random[RR],
        rs: Random[SS],
        rt: Random[TT]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W],
      rp.widen[W],
      rq.widen[W],
      rr.widen[W],
      rs.widen[W],
      rt.widen[W]
    )
  }
  final class ApplyOneOf21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W,
        PP >: P <: W,
        QQ >: Q <: W,
        RR >: R <: W,
        SS >: S <: W,
        TT >: T <: W,
        UU >: U <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO],
        rp: Random[PP],
        rq: Random[QQ],
        rr: Random[RR],
        rs: Random[SS],
        rt: Random[TT],
        ru: Random[UU]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W],
      rp.widen[W],
      rq.widen[W],
      rr.widen[W],
      rs.widen[W],
      rt.widen[W],
      ru.widen[W]
    )
  }
  final class ApplyOneOf22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](private val dummy: Boolean = true)
      extends AnyVal {
    def make[
        W,
        AA >: A <: W,
        BB >: B <: W,
        CC >: C <: W,
        DD >: D <: W,
        EE >: E <: W,
        FF >: F <: W,
        GG >: G <: W,
        HH >: H <: W,
        II >: I <: W,
        JJ >: J <: W,
        KK >: K <: W,
        LL >: L <: W,
        MM >: M <: W,
        NN >: N <: W,
        OO >: O <: W,
        PP >: P <: W,
        QQ >: Q <: W,
        RR >: R <: W,
        SS >: S <: W,
        TT >: T <: W,
        UU >: U <: W,
        VV >: V <: W
    ](implicit
        ra: Random[AA],
        rb: Random[BB],
        rc: Random[CC],
        rd: Random[DD],
        re: Random[EE],
        rf: Random[FF],
        rg: Random[GG],
        rh: Random[HH],
        ri: Random[II],
        rj: Random[JJ],
        rk: Random[KK],
        rl: Random[LL],
        rm: Random[MM],
        rn: Random[NN],
        ro: Random[OO],
        rp: Random[PP],
        rq: Random[QQ],
        rr: Random[RR],
        rs: Random[SS],
        rt: Random[TT],
        ru: Random[UU],
        rv: Random[VV]
    ): Random[W] = oneOfRandomImpl(
      ra.widen[W],
      rb.widen[W],
      rc.widen[W],
      rd.widen[W],
      re.widen[W],
      rf.widen[W],
      rg.widen[W],
      rh.widen[W],
      ri.widen[W],
      rj.widen[W],
      rk.widen[W],
      rl.widen[W],
      rm.widen[W],
      rn.widen[W],
      ro.widen[W],
      rp.widen[W],
      rq.widen[W],
      rr.widen[W],
      rs.widen[W],
      rt.widen[W],
      ru.widen[W],
      rv.widen[W]
    )
  }
}
