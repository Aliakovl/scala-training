package dev.aliakovl.gin
package macros

import scala.collection.BuildFrom

private[macros] final class State[S, +A](val run: S => (S, A)) extends AnyVal {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (s1, a) = run(s)
    (s1, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (s1, a) = run(s)
    f(a).run(s1)
  }

  def as[B](b: B): State[S, B] = State { s =>
    val (s1, _) = run(s)
    (s1, b)
  }

  def zip[B](other: => State[S, B]): State[S, (A, B)] = State { s =>
    val (s1, a) = run(s)
    val (s2, b) = other.run(s1)
    (s2, (a, b))
  }

  def <*[B](other: => State[S, B]): State[S, A] = State { s =>
    val (s1, a) = run(s)
    val (s2, _) = other.run(s1)
    (s2, a)
  }

  def *>[B](other: => State[S, B]): State[S, B] = State { s =>
    val (s1, _) = run(s)
    val (s2, b) = other.run(s1)
    (s2, b)
  }

  def flatTap[B](f: A => State[S, B]): State[S, A] = State { s =>
    val (s1, a) = run(s)
    val (s2, _) = f(a).run(s1)
    (s2, a)
  }

  def modifyState[T](implicit lens: Lens[T, S]): State[T, A] = State { t =>
    val s = lens.get(t)
    val (s1, a) = run(s)
    val t1 = lens.set(t, s1)
    (t1, a)
  }

  def fallback[B](f: => State[S, B])(implicit ev: A <:< Option[B]): State[S, B] = State { s =>
    val (s1, a) = run(s)
    ev(a) match {
      case Some(value) => (s1, value)
      case None => f.run(s)
    }
  }

  def eval(init: S): S = run(init)._1
}

private[macros] object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A](f)
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def unit[S]: State[S, Unit] = State(s => (s, ()))
  def none[S, A]: State[S, Option[A]] = State(s => (s, None))
  def some[S, A](a: A): State[S, Option[A]] = State(s => (s, Some(a)))
  def get[S]: State[S, S] = State(s => (s, s))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
  def modifyFirst[S, T](f: S => S): State[(S, T), Unit] =
    State(p => ((f(p._1), p._2), ()))
  def modifySecond[S, T](f: T => T): State[(S, T), Unit] =
    State(p => ((p._1, f(p._2)), ()))
  def traverse[S, C[+E] <: IterableOnce[E], A, B](ta: C[A])(
      f: A => State[S, B]
  )(implicit bf: BuildFrom[C[A], B, C[B]]): State[S, C[B]] = {
    val iterator = ta.iterator
    val builder = bf.newBuilder(ta)

    State.apply[S, C[B]] { initState =>
      var state = initState
      while (iterator.hasNext) {
        val v = iterator.next()
        val (s, b) = f(v).run(state)
        state = s
        builder += b
      }

      (state, builder.result())
    }
  }

  def sequence[C[+E] <: IterableOnce[E], S, A](ta: C[State[S, A]])(implicit
      bf: BuildFrom[C[State[S, A]], A, C[A]]
  ): State[S, C[A]] = traverse(ta)(identity)

  def sequence[S, A](ta: Option[State[S, A]]): State[S, Option[A]] =
    ta.fold[State[S, Option[A]]](State.pure(None))(_.map(Some(_)))

  def traverse[S, A, B](ta: Option[A])(f: A => State[S, B]): State[S, Option[B]] = sequence(ta.map(f))

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
