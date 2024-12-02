package dev.aliakovl.gin

import dev.aliakovl.gin.internal._

import scala.annotation.compileTimeOnly
import scala.collection.Factory
import scala.language.implicitConversions
import scala.util.Random

final class Gen[A] private (private[gin] val unsafeRun: Random => A) extends AnyVal {

  def apply(random: Random): A = unsafeRun(random)

  def runWithCtx(implicit random: Random): A = unsafeRun(random)

  def runWithSeed(seed: Long): A = unsafeRun(new Random(seed))

  def run(): A = unsafeRun(Random)

  def map[B](f: A => B): Gen[B] = Gen { random =>
    f(unsafeRun(random))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen { random =>
    f(unsafeRun(random)).unsafeRun(random)
  }

  def tap(f: A => Any): Gen[A] = Gen { random =>
    val r = unsafeRun(random); f(r); r
  }

  def widen[T >: A]: Gen[T] = this.asInstanceOf[Gen[T]]

  def many[C[E] <: IterableOnce[E]](size: Int)(implicit
      factory: Factory[A, C[A]]
  ): Gen[C[A]] = Gen { random =>
    factory.fromSpecific(Iterable.fill(size)(unsafeRun(random)))
  }

  def toMap[K, V](size: Int)(implicit ev: A <:< (K, V)): Gen[Map[K, V]] =
    Gen { random =>
      Iterable.fill(size)(unsafeRun(random)).toMap
    }

  def stream: Gen[LazyList[A]] = Gen { random =>
    LazyList.continually(unsafeRun(random))
  }
}

object Gen
    extends GenCats
    with GenInstances
    with GenOneOf {

  def apply[A](eval: Random => A): Gen[A] = new Gen(eval)

  def make[A](eval: => A): Gen[A] = new Gen(_ => eval)

  def const[A](value: A): Gen[A] = make(value)

  def random[A](implicit inst: Gen[A]): Gen[A] = inst

  @compileTimeOnly("Illegal reference to dev.aliakovl.gin.Gen.custom, try to call .make after it")
  def custom[A]: GenCustom[A] = ???

  def fromFunction[In](in: In)(implicit
      constructor: FunctionConstructor[In]
  ): constructor.Out = constructor(in)

  def function[A, B](f: A => Gen[B]): Gen[A => B] = Gen { random => in =>
    f(in).unsafeRun(random)
  }

  def product[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = Gen { random =>
    (ga.unsafeRun(random), gb.unsafeRun(random))
  }

  def uglyString(size: Int): Gen[String] = Gen {
    _.nextString(size)
  }

  def string(size: Int): Gen[String] = {
    Gen.charGen.many[LazyList](size).map(_.mkString)
  }

  def alphanumeric(size: Int): Gen[String] = Gen {
    _.alphanumeric.take(size).mkString
  }

  def between(minInclusive: Int, maxExclusive: Int): Gen[Int] = Gen {
    _.between(minInclusive, maxExclusive)
  }

  def between(minInclusive: Long, maxExclusive: Long): Gen[Long] = Gen {
    _.between(minInclusive, maxExclusive)
  }
}
