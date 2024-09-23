package dev.aliakovl.gin

import dev.aliakovl.gin.internal._

import scala.language.implicitConversions
import scala.util.Random

final class Gen[A] private (private val run: Random => A) {
  def apply(inst: Random): A = run(inst)
  def unsafe(): A = run(Random)
  def runWithSeed(seed: Long): A = run(new Random(seed))

  def map[B](f: A => B): Gen[B] = Gen { r =>
    f(run(r))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen { r =>
    f(run(r)).run(r)
  }

  def tap[U](f: A => U): Gen[A] = Gen { rand =>
    val r = run(rand)
    f(r)
    r
  }

  def widen[T >: A]: Gen[T] = this.asInstanceOf[Gen[T]]
}

object Gen
    extends GenLowPriority
    with GenDerivation
    with GenInstances
    with GenOneOf {

  def apply[A](eval: Random => A): Gen[A] = new Gen(eval)

  def apply[A](eval: => A): Gen[A] = new Gen(_ => eval)

  def random[A](implicit inst: Gen[A]): Gen[A] = inst

  def const[A](value: A): Gen[A] = apply(value)

  def custom[A]: GenSpecify[A] = new GenSpecify[A]

  def fromFunction[In](in: In)(implicit
      constructor: FunctionConstructor[In]
  ): constructor.Out = constructor(in)

  def product[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = Gen { r =>
    (ga.run(r), gb.run(r))
  }

  def uglyString(size: Int): Gen[String] = Gen {
    _.nextString(size)
  }

  def string(size: Int): Gen[String] = {
    Gen.charGen.many[LazyList](size).map(_.mkString)
  }

  def alphanumeric(size: Int): Gen[String] = apply {
    _.alphanumeric.take(size).mkString
  }

  def between(minInclusive: Int, maxExclusive: Int): Gen[Int] = apply {
    _.between(minInclusive, maxExclusive)
  }

  def enumeration[E <: Enumeration](
      en: E
  ): Gen[E#Value] = Gen { r =>
    en(r.nextInt(en.maxId))
  }
}
