package dev.aliakovl.core.data

//import dev.aliakovl.core.data.Coproduct.SelectPartiallyApplied

sealed trait Coproduct {
//  def select[V]: SelectPartiallyApplied[V]
}

object Coproduct {
  def apply[T <: Coproduct] = new PartiallyApplied[T]()

  final class PartiallyApplied[T <: Coproduct](val dummy: Boolean = true)
      extends AnyVal {
    def apply[V](v: V)(implicit ev: T Contains V): T = ev(v)
  }

//  trait SelectPartiallyApplied[V] {
//    def apply[T](implicit ev: T Contains V): Option[V]
//  }
}

sealed trait :+:[H, T <: Coproduct] extends Coproduct {
  def select[V]: Option[V]
}

sealed trait CNil extends Coproduct {
  def impossible: Nothing
}

trait Contains[C <: Coproduct, A] {
  def apply(a: A): C
}

object Contains {
  implicit def basic[T <: Coproduct, A]: (A :+: T) Contains A =
    new Contains[A :+: T, A] {
      override def apply(a: A): A :+: T = new :+:[A, T] {
        override def select[V]: Option[V] = {
          a match {
            case v: V =>
              Some(v)
            case _ =>
              None
          }
        }
      }
    }
  implicit def inductive[T <: Coproduct, H, A](implicit
      ev: T Contains A
  ): (H :+: T) Contains A = new Contains[H :+: T, A] {
    override def apply(a: A): H :+: T = new :+:[H, T] {
      override def select[V]: Option[V] = {
        None
      }
    }
  }

  def apply[C <: Coproduct, A](implicit ev: C Contains A): C Contains A = ev
}
