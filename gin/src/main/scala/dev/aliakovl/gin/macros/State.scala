package dev.aliakovl.gin.macros

import scala.collection.BuildFrom

private[macros] final class State[S, +A](val run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (newS, a) = run(s)
    (newS, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (newS, a) = run(s)
    f(a).run(newS)
  }

  def as[B](b: B): State[S, B] = State { s =>
    val (newS, _) = run(s)
    (newS, b)
  }

  def zip[B](other: State[S, B]): State[S, (A, B)] = State { s =>
    val (s1, a) = run(s)
    val (s2, b) = other.run(s1)
    (s2, (a, b))
  }

  def *>[B](right: State[S, B]): State[S, B] = flatMap(_ => right)

  def <*[B](right: State[S, B]): State[S, A] = flatMap(a => right.as(a))

  def modifyState[T](to: S => T)(from: T => S): State[T, A] = State { t =>
    val (s, a) = run(from(t))
    (to(s), a)
  }

  def widen[T]: State[(T, S), A] = State { case (t, s) =>
    val (s1, a) = run(s)
    ((t, s1), a)
  }

  def runPartially[T, S1](t: T)(implicit ev: S =:= (T, S1)): State[S1, A] = State.apply[S1, A] { s1 =>
    val s: S = ev.flip((t, s1))
    val (newS, a) = run(s)
    (ev.apply(newS)._2, a)
  }
}

private[macros] object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A](f)
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
  def modify_1[S, T](f: S => S): State[(S, T), Unit] =
    State(p => ((f(p._1), p._2), ()))
  def modify_2[S, T](f: T => T): State[(S, T), Unit] =
    State(p => ((p._1, f(p._2)), ()))
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def traverse[S, C[+E] <: Iterable[E], A, B](ta: C[A])(
      f: A => State[S, B]
  )(implicit bf: BuildFrom[C[A], B, C[B]]): State[S, C[B]] = {
    val iterator = ta.iterator
    val builder = bf.newBuilder(ta)

    State.apply[S, C[B]] { initState =>
      var state = initState
      while (iterator.hasNext) {
        val v = iterator.next()
        val res: (S, B) = f(v).run(state)
        state = res._1
        builder += res._2
      }

      (state, builder.result())
    }
  }

  def traverse[S, A, B](ta: Set[A])(
      f: A => State[S, B]
  ): State[S, Set[B]] = {
    ta.foldLeft(State.pure[S, Set[B]](Set.empty)) { case (acc, value) =>
      for {
        set <- acc
        b <- f(value)
      } yield set + b
    }
  }

  def traverse[S, K, KK, V, VV](ta: Map[K, V])(
      f: (K, V) => State[S, (KK, VV)]
  ): State[S, Map[KK, VV]] = {
    ta.foldLeft(State.pure[S, Map[KK, VV]](Map.empty)) { case (acc, value) =>
      for {
        map <- acc
        b <- f.tupled(value)
      } yield map + b
    }
  }
}

private[macros] object MState {
  def getOrElseUpdate[K, V](key: K, value: => V): State[Map[K, V], V] = {
    for {
      s <- State.get[Map[K, V]]
      v <- s.get(key) match {
        case Some(v) => State.pure[Map[K, V], V](v)
        case None =>
          val v = value
          State.modify[Map[K, V]](_.updated(key, v)).as(v)
      }
    } yield v
  }
}
