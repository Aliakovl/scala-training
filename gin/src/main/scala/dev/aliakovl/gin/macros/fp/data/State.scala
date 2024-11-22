package dev.aliakovl.gin.macros.fp.data

import dev.aliakovl.gin.macros.fp.Applicative
import dev.aliakovl.gin.macros.fp.optics.Lens

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

  def unit: State[S, Unit] = as(())

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

  def fallback[B](
      f: => State[S, B]
  )(implicit ev: A <:< Option[B]): State[S, B] = State { s =>
    val (s1, a) = run(s)
    ev(a) match {
      case Some(value) => (s1, value)
      case None        => f.run(s)
    }
  }

  def eval(init: S): S = run(init)._1
}

private[macros] object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A](f)
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
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

  def modifyUnless[S](pred: S => Boolean)(f: S => S): State[S, Unit] = State(s =>
    if (pred(s)) {
      (s, ())
    } else {
      (f(s), ())
    }
  )

  implicit def applicationForState[S]
      : Applicative[({ type M[A] = State[S, A] })#M] =
    new Applicative[({ type M[A] = State[S, A] })#M] {
      override def pure[A](a: A): State[S, A] = State.pure(a)

      override def ap[A, B](
          ff: State[S, A => B]
      )(fa: State[S, A]): State[S, B] = State { s =>
        val (s1, f) = ff.run(s)
        val (s2, a) = fa.run(s1)
        (s2, f(a))
      }
    }
}
