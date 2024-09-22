package dev.aliakovl.gin

import dev.aliakovl.gin.internal._

import scala.language.implicitConversions
import scala.util.Random

final class Gen[A] private (private val run: () => A) {
  def apply(): A = run()

  def map[B](f: A => B): Gen[B] = Gen(f(run()))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(f(run()).run())

  def foreach[U](f: A => U): Unit = f(run())

  def tap[U](f: A => U): Gen[A] = Gen {
    val r = run()
    f(r)
    r
  }

  def widen[T >: A]: Gen[T] = this.asInstanceOf[Gen[T]]
}

object Gen extends GenLowPriority with GenDerivation with GenInstances with GenOneOf {

  def apply[A](eval: => A): Gen[A] = new Gen(() => eval)

  def random[A](implicit inst: Gen[A]): Gen[A] = inst

  def const[A](value: A): Gen[A] = apply(value)

  def custom[A]: GenSpecify[A] = new GenSpecify[A]

  def fromFunction[In](in: In)(implicit
      constructor: FunctionConstructor[In]
  ): constructor.Out = constructor(in)

  def product[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = Gen((ga(), gb()))

  def uglyString(size: Int): Gen[String] = apply {
    Random.nextString(size)
  }

  def string(size: Int): Gen[String] = {
    Gen.charGen.many[LazyList](size).map(_.mkString)
  }

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
