package dev.aliakovl.kernel

type Compose[F[_], G[_]] = [A] =>> F[G[A]]

object Compose:
  given [F[_]: Applicative, G[_]: Applicative]: Applicative[Compose[F, G]] with
    def pure[A](a: A): F[G[A]] =
      summon[Applicative[F]].pure(summon[Applicative[G]].pure(a))

    extension [A, B, FG[T] <: F[G[T]]](fg: FG[A => B])
      def ap(fa: F[G[A]]): F[G[B]] = summon[Applicative[F]].map2(fg, fa) {
        (gf, ga) =>
          gf.ap(ga)
      }
