package dev.aliakovl.gin

import dev.aliakovl.gin.internal.{
  ManyRandom,
  OneOfRandom,
  RandomDerivation
}

import scala.language.implicitConversions
import scala.util.{Random => ScalaRandom}

trait Random[A] { self =>
  def get(): A

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

  def apply[A](eval: => A): Random[A] = () => eval

  def apply[A](implicit inst: Random[A]): Random[A] = inst

  def const[A](value: A): Random[A] = () => value

  def random[A: Random]: Random[A] = Random[A]

  def uglyString(size: Int): Random[String] = Random {
    ScalaRandom.nextString(size)
  }

  def string(size: Int): Random[String] =
    many[LazyList](size).make[Char].map(_.mkString)

  def alphanumeric(size: Int): Random[String] = Random {
    ScalaRandom.alphanumeric.take(size).mkString
  }

}
