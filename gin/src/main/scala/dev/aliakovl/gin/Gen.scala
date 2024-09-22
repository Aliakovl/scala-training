package dev.aliakovl.gin

import dev.aliakovl.gin.internal._

import scala.language.implicitConversions
import scala.util.Random

trait Gen[A] extends (() => A) {
  override def apply(): A

  def map[B](f: A => B): Gen[B] = Gen(f(apply()))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(f(apply()).apply())

  def product[B](fb: Gen[B]): Gen[(A, B)] = Gen((apply(), fb.apply()))

  def foreach[U](f: A => U): Unit = f(apply())

  def tap[U](f: A => U): Gen[A] = Gen {
    val r = apply()
    f(r)
    r
  }

  def widen[T >: A]: Gen[T] = this.asInstanceOf[Gen[T]]
}

object Gen
    extends GenDerivation
    with GenInstances
    with GenOneOf
    with GenMany {

  def apply[A](eval: => A): Gen[A] = () => eval

  def random[A](implicit inst: Gen[A]): Gen[A] = inst

  def const[A](value: A): Gen[A] = apply(value)

  def custom[A]: GenSpecify[A] = new GenSpecify[A]

  def fromFunction[In](in: In)(implicit
      constructor: FunctionConstructor[In]
  ): constructor.Out = constructor(in)

  def uglyString(size: Int): Gen[String] = apply {
    Random.nextString(size)
  }

  def string(size: Int): Gen[String] =
    many[LazyList](size).make[Char].map(_.mkString)

  def alphanumeric(size: Int): Gen[String] = apply {
    Random.alphanumeric.take(size).mkString
  }

  def between(minInclusive: Int, maxExclusive: Int): Gen[Int] = apply {
    Random.between(minInclusive, maxExclusive)
  }

  def enumeration[E <: Enumeration](
      en: E
  ): Gen[E#Value] = Gen {
    en(Random.nextInt(en.maxId))
  }

}
