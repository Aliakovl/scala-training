package dev.aliakovl.gin.macros.fp.data

import dev.aliakovl.gin.macros.fp.{Semigroup, SemigroupK}
import dev.aliakovl.gin.macros.fp.data.Chain._

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class Chain[+A] extends IterableOnce[A] {
  override def iterator: Iterator[A] = new ChainIterator[A](this)

  def concat[A1 >: A](chain: Chain[A1]): Chain[A1] =
    Merge(this, chain)

  def append[A1 >: A](value: A1): Chain[A1] =
    Merge(this, Chain.one(value))
}

object Chain {
  final case class Singleton[A](value: A) extends Chain[A]
  final case class Merge[A](left: Chain[A], right: Chain[A]) extends Chain[A]

  def one[A](a: A): Chain[A] = Singleton(a)

  private class ChainIterator[A](self: Chain[A]) extends Iterator[A] {
    private val stack: mutable.Stack[Chain[A]] = mutable.Stack(self)

    @tailrec
    private def go(chain: Chain[A]): A = {
      chain match {
        case Singleton(value) => value
        case Merge(left, right) =>
          stack.push(right)
          go(left)
      }
    }

    override def hasNext: Boolean = stack.nonEmpty

    override def next(): A = {
      if (!hasNext)
        throw new NoSuchElementException("There are no elements to iterate")
      go(stack.pop())
    }
  }

  implicit val semigroupKForChain: SemigroupK[Chain] = new SemigroupK[Chain] {
    override def combineK[A](a: Chain[A], b: Chain[A]): Chain[A] = a.concat(b)
  }

  implicit def semigroupForChain[A]: Semigroup[Chain[A]] = semigroupKForChain.combineK[A]
}
