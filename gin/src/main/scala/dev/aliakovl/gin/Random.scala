package dev.aliakovl.gin

import dev.aliakovl.gin.internal._

import scala.language.implicitConversions
import scala.util.{Random => ScalaRandom}

final class Random[A](val get: () => A) extends AnyVal {
  def map[B](f: A => B): Random[B] = Random(f(get()))

  def flatMap[B](f: A => Random[B]): Random[B] = Random(f(get()).get())

  def product[B](fb: Random[B]): Random[(A, B)] = Random((get(), fb.get()))

  def widen[T >: A]: Random[T] = Random(get())
}

object Random
    extends GeneratorInstances
    with RandomDerivation
    with OneOfRandom
    with ManyRandom {

  def apply[A](eval: => A): Random[A] = new Random[A](() => eval)

  def random[A](implicit inst: Random[A]): Random[A] = inst

  def const[A](value: A): Random[A] = apply(value)

  def uglyString(size: Int): Random[String] = apply {
    ScalaRandom.nextString(size)
  }

  def string(size: Int): Random[String] =
    many[LazyList](size).make[Char].map(_.mkString)

  def alphanumeric(size: Int): Random[String] = apply {
    ScalaRandom.alphanumeric.take(size).mkString
  }

  def enumeration[E <: Enumeration](
      en: E
  ): Random[E#Value] = Random {
    en(ScalaRandom.nextInt(en.maxId))
  }
}
