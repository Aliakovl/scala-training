package dev.aliakovl.gin

import dev.aliakovl.gin.Random.random
import dev.aliakovl.gin.internal.{ManyRandom, OneOfRandom, RandomDerivation}

import scala.language.implicitConversions
import scala.util.{Random => ScalaRandom}

case class Random[A](get: () => A) {
  def map[B](f: A => B): Random[B] = random(f(get()))

  def flatMap[B](f: A => Random[B]): Random[B] = random(f(get()).get())

  def product[B](fb: Random[B]): Random[(A, B)] = random((get(), fb.get()))

  def widen[T >: A]: Random[T] = random(get())
}

object Random
    extends GeneratorInstances
    with RandomDerivation
    with OneOfRandom
    with ManyRandom {

  def apply[A](implicit inst: Random[A]): Random[A] = inst

  def random[A: Random]: Random[A] = Random[A]

  def random[A](eval: => A): Random[A] = Random(() => eval)

  def const[A](value: A): Random[A] = Random(() => value)

  def uglyString(size: Int): Random[String] = random {
    ScalaRandom.nextString(size)
  }

  def string(size: Int): Random[String] =
    many[LazyList](size).make[Char].map(_.mkString)

  def alphanumeric(size: Int): Random[String] = random {
    ScalaRandom.alphanumeric.take(size).mkString
  }

}
