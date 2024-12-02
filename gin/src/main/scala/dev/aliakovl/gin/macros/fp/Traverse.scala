package dev.aliakovl.gin.macros.fp

import scala.collection.mutable.ListBuffer

trait Traverse[T[_]] {
  def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]]
}

object Traverse {
  final class TraverseOps[T[_], A](private val ta: T[A]) extends AnyVal {
    def traverse[F[_]: Applicative, B](
        f: A => F[B]
    )(implicit T: Traverse[T]): F[T[B]] = {
      T.traverse(ta)(f)
    }
  }

  implicit val traverseForList: Traverse[List] = new Traverse[List] {
    override def traverse[F[_], A, B](ta: List[A])(
        f: A => F[B]
    )(implicit F: Applicative[F]): F[List[B]] = F.map {
      ta.foldLeft(F.pure(ListBuffer.empty[B])) { case (accF, a) =>
        F.map2(accF, f(a)) { case (acc, b) =>
          acc += b
        }
      }
    }(_.toList)
  }

  implicit val traverseForOption: Traverse[Option] = new Traverse[Option] {
    override def traverse[F[_], A, B](ta: Option[A])(
        f: A => F[B]
    )(implicit F: Applicative[F]): F[Option[B]] = {
      ta.fold(F.pure[Option[B]](None)) { value =>
        F.map(f(value))(Some.apply)
      }
    }
  }

  implicit val traverseForSet: Traverse[Set] = new Traverse[Set] {
    override def traverse[F[_], A, B](
        ta: Set[A]
    )(f: A => F[B])(implicit F: Applicative[F]): F[Set[B]] = {
      ta.foldLeft(F.pure(Set.empty[B])) { case (accF, a) =>
        F.map2(accF, f(a)) { case (acc, b) =>
          acc + b
        }
      }
    }
  }

  implicit def traverseForIterator: Traverse[Iterator] =
    new Traverse[Iterator] {
      override def traverse[F[_], A, B](
          ta: Iterator[A]
      )(f: A => F[B])(implicit F: Applicative[F]): F[Iterator[B]] = {
        F.map {
          ta.iterator.foldLeft(F.pure(Iterator.newBuilder[B])) {
            case (accF, a) =>
              F.map2(accF, f(a)) { case (acc, b) =>
                acc.addOne(b)
              }
          }
        }(_.result())
      }
    }
}
