package dev.aliakovl.monad

import dev.aliakovl.monad.data.{<=, Iso, Op}

trait Category[->[_, _]]:
  def id[A]: A -> A
  def andThen[A, B, C](f: A -> B, g: B -> C): A -> C

object Category:
  given Category[Function] with
    override def andThen[A, B, C](f: A => B, g: B => C): A => C = f andThen g
    override def id[A]: A => A = identity

  given Category[<:<] with
    override def andThen[A, B, C](f: A <:< B, g: B <:< C): A <:< C = f andThen g
    override def id[A]: A <:< A = <:<.refl

  given Category[Op] with
    override def andThen[A, B, C](f: A <= B, g: B <= C): A <= C = Op(
      g.run andThen f.run
    )
    override def id[A]: A <= A = Op(identity)

  given Category[Iso] with
    override def andThen[A, B, C](f: Iso[A, B], g: Iso[B, C]): Iso[A, C] =
      Iso[A, C](
        f = f.f andThen g.f,
        g = g.g andThen f.g
      )
    override def id[A]: Iso[A, A] = Iso(identity, identity)
