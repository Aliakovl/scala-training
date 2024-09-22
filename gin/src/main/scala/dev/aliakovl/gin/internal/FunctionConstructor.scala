package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Gen

trait FunctionConstructor[In] {
  type Out
  def apply(in: In): Out
}

object FunctionConstructor {
  type Aux[In, Out0] = FunctionConstructor[In] { type Out = Out0 }

  implicit def function0Constructor[W]: FunctionConstructor.Aux[() => W, Gen[W]] =
    new FunctionConstructor[() => W] {
      override type Out = Gen[W]
      override def apply(in: () => W): Gen[W] = Gen(in())
    }

  implicit def function1Constructor[A, W](implicit a: Gen[A]): FunctionConstructor.Aux[A => W, Gen[W]] =
    new FunctionConstructor[A => W] {
      override type Out = Gen[W]
      override def apply(in: A => W): Gen[W] = Gen(in(a()))
    }

  implicit def function2Constructor[A, B, W](implicit a: Gen[A], b: Gen[B]): FunctionConstructor.Aux[(A, B) => W, Gen[W]] =
    new FunctionConstructor[(A, B) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B) => W): Gen[W] = Gen(in(a(), b()))
    }

  implicit def function3Constructor[A, B, C, W](implicit a: Gen[A], b: Gen[B], c: Gen[C]): FunctionConstructor.Aux[(A, B, C) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C) => W): Gen[W] = Gen(in(a(), b(), c()))
    }

  implicit def function4Constructor[A, B, C, D, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D]): FunctionConstructor.Aux[(A, B, C, D) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D) => W): Gen[W] = Gen(in(a(), b(), c(), d()))
    }

  implicit def function5Constructor[A, B, C, D, E, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E]): FunctionConstructor.Aux[(A, B, C, D, E) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e()))
    }

  implicit def function6Constructor[A, B, C, D, E, F, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F]): FunctionConstructor.Aux[(A, B, C, D, E, F) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f()))
    }

  implicit def function7Constructor[A, B, C, D, E, F, G, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G]): FunctionConstructor.Aux[(A, B, C, D, E, F, G) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g()))
    }

  implicit def function8Constructor[A, B, C, D, E, F, G, H, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h()))
    }

  implicit def function9Constructor[A, B, C, D, E, F, G, H, I, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i()))
    }

  implicit def function10Constructor[A, B, C, D, E, F, G, H, I, J, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j()))
    }

  implicit def function11Constructor[A, B, C, D, E, F, G, H, I, J, K, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k()))
    }

  implicit def function12Constructor[A, B, C, D, E, F, G, H, I, J, K, L, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l()))
    }

  implicit def function13Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m()))
    }

  implicit def function14Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n()))
    }

  implicit def function15Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o()))
    }

  implicit def function16Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o(), p()))
    }

  implicit def function17Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o(), p(), q()))
    }

  implicit def function18Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o(), p(), q(), r()))
    }

  implicit def function19Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o(), p(), q(), r(), s()))
    }

  implicit def function20Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o(), p(), q(), r(), s(), t()))
    }

  implicit def function21Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o(), p(), q(), r(), s(), t(), u()))
    }

  implicit def function22Constructor[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit a: Gen[A], b: Gen[B], c: Gen[C], d: Gen[D], e: Gen[E], f: Gen[F], g: Gen[G], h: Gen[H], i: Gen[I], j: Gen[J], k: Gen[K], l: Gen[L], m: Gen[M], n: Gen[N], o: Gen[O], p: Gen[P], q: Gen[Q], r: Gen[R], s: Gen[S], t: Gen[T], u: Gen[U], v: Gen[V]): FunctionConstructor.Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W, Gen[W]] =
    new FunctionConstructor[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] {
      override type Out = Gen[W]
      override def apply(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W): Gen[W] = Gen(in(a(), b(), c(), d(), e(), f(), g(), h(), i(), j(), k(), l(), m(), n(), o(), p(), q(), r(), s(), t(), u(), v()))
    }
}
