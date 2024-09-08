package dev.aliakovl.gin.macros

private[macros] final class State[S, A](val run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (newS, a) = run(s)
    (newS, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (newS, a) = run(s)
    f(a).run(newS)
  }
}

private[macros] object State {
  def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A](f)
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
}
