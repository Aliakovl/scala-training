package dev.aliakovl.gin

import dev.aliakovl.gin.internal.{GenericRandom, ManyRandom, OneOfRandom}

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
    with GenericRandom
    with OneOfRandom
    with ManyRandom {

  def apply[A](const: => A): Random[A] = () => const

  def apply[A](implicit g: Random[A]): Random[A] = g

  def random[A: Random]: Random[A] = Random[A]

  def uglyString(size: Int): Random[String] = Random(ScalaRandom.nextString(size))

  def string(size: Int): Random[String] = many[LazyList](size).make[Char].map(_.mkString)

  def alphanumeric(size: Int): Random[String] = Random(ScalaRandom.alphanumeric.take(size).mkString)

}
