package dev.aliakovl.gin
package internal

trait FunctionConstructor[In] {
  type Out
  def apply(in: In): Out
}

object FunctionConstructor {
  type Aux[In, Out0] = FunctionConstructor[In] { type Out = Out0 }

  implicit def function0Constructor[W]: Aux[() => W, Gen[W]] =
    new FunctionConstructor[() => W] {
      override type Out = Gen[W]
      override def apply(in: () => W): Gen[W] = Gen(_ => in())
    }

  implicit def function1Constructor[A, W](implicit a: Gen[A]): Aux[A => W, Gen[W]] =
    new FunctionConstructor[A => W] {
      override type Out = Gen[W]
      override def apply(in: A => W): Gen[W] = Gen(x => in(a(x)))
    }

  implicit def function2Constructor[A, B, W](implicit a: Gen[A], b: Gen[B]): Aux[(A, B) => W, Gen[W]] =
    new FunctionConstructor[(A, B) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B) => W): Gen[W] = Gen(x => in(a(x), b(x)))
    }

  implicit def function3Constructor[A, B, C, W](implicit a: Gen[A], b: Gen[B], c: Gen[C]): Aux[(A, B, C) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C) => W): Gen[W] = Gen(x =>  in(a(x), b(x), c(x)))
    }

  implicit def function4Constructor[A, B, C, D, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D]): Aux[(A, B, C, D) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x)))
    }

  implicit def function5Constructor[A, B, C, D, E, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E]): Aux[(A, B, C, D, E) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x)))
    }

  implicit def function6Constructor[A, B, C, D, E, F, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F]): Aux[(A, B, C, D, E, F) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x)))
    }

  implicit def function7Constructor[A, B, C, D, E, F, G, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G]): Aux[(A, B, C, D, E, F, G) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x)))
    }

  implicit def function8Constructor[A, B, C, D, E, F, G, H, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H]): Aux[(A, B, C, D, E, F, G, H) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x)))
    }

  implicit def function9Constructor[A, B, C, D, E, F, G, H, I, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I]): Aux[(A, B, C, D, E, F, G, H, I) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x)))
    }

  implicit def function10Constructor[A, B, C, D, E, F, G, H, I, J, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J]): Aux[(A, B, C, D, E, F, G, H, I, J) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x)))
    }

  implicit def function11Constructor[A, B, C, D, E, F, G, H, I, J, K, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K]): Aux[(A, B, C, D, E, F, G, H, I, J, K) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x)))
    }

  implicit def function12Constructor[A, B, C, D, E, F, G, H, I, J, K, L, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x)))
    }

  implicit def function13Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x)))
    }

  implicit def function14Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x)))
    }

  implicit def function15Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x)))
    }

  implicit def function16Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x), p(x)))
    }

  implicit def function17Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x), p(x), q(x)))
    }

  implicit def function18Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x), p(x), q(x), r(x)))
    }

  implicit def function19Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x), p(x), q(x), r(x), s(x)))
    }

  implicit def function20Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x), p(x), q(x), r(x), s(x), t(x)))
    }

  implicit def function21Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x), p(x), q(x), r(x), s(x), t(x), u(x)))
    }

  implicit def function22Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U], v: Gen[V]): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W): Gen[W] = Gen(x => in(a(x), b(x), c(x), d(x), e(x), f(x), g(x), h(x), i(x), j(x), k(x), l(x), m(x), n(x), o(x), p(x), q(x), r(x), s(x), t(x), u(x), v(x)))
    }
}
