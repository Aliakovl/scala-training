package dev.aliakovl.gin

import dev.aliakovl.gin.internal._

import scala.language.implicitConversions
import scala.util.{Random => ScalaRandom}

trait Random[A] extends (() => A) {
  override def apply(): A

  def map[B](f: A => B): Random[B] = Random(f(apply()))

  def flatMap[B](f: A => Random[B]): Random[B] = Random(f(apply()).apply())

  def product[B](fb: Random[B]): Random[(A, B)] = Random((apply(), fb.apply()))

  def withFilter(p: A => Boolean): Random[Option[A]] = {
    val t = apply()
    Random(Option.when(p(t))(t))
  }

  def widen[T >: A]: Random[T] = this.asInstanceOf[Random[T]]
}

object Random
    extends RandomDerivation
    with RandomInstances
    with OneOfRandom
    with ManyRandom {

  def apply[A](eval: => A): Random[A] = () => eval

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
