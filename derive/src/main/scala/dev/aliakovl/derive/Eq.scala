package dev.aliakovl.derive

import scala.collection.AbstractIterable
import scala.deriving.*
import scala.compiletime.{erasedValue, error, summonInline}

inline def summonInstances[T, Elems <: Tuple]: List[Eq[?]] =
  inline erasedValue[Elems] match
    case _: (elem *: elems) => deriveOrSummon[T, elem] :: summonInstances[T, elems]
    case _: EmptyTuple => Nil

inline def deriveOrSummon[T, Elem]: Eq[Elem] =
  inline erasedValue[Elem] match
    case _: T => deriveRec[T, Elem]
    case _    => summonInline[Eq[Elem]]

inline def deriveRec[T, Elem]: Eq[Elem] =
  inline erasedValue[T] match
    case _: Elem => error("infinite recursive derivation")
    case _       => Eq.derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation

trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  given Eq[Int] with
    def eqv(x: Int, y: Int) = x == y

  def check(x: Any, y: Any, elem: Eq[?]): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterable[T](p: T): Iterable[Any] = new AbstractIterable[Any]:
    def iterator: Iterator[Any] = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(x, y, elems(ordx))

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        iterable(x).lazyZip(iterable(y)).lazyZip(elems).forall(check)

  inline def derived[T](using m: Mirror.Of[T]): Eq[T] =
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
end Eq
