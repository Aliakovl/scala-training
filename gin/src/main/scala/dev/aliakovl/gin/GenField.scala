package dev.aliakovl.gin

import scala.language.implicitConversions

sealed trait GenField[+A] {
  def create[B >: A : Random]: B = this match {
    case RandomField           => Random[B].get()
    case SpecifiedField(value) => value
  }
  def create[B >: A](random: => B): B = this match {
    case RandomField           => random
    case SpecifiedField(value) => value
  }
}

case object RandomField extends GenField[Nothing]
case class SpecifiedField[+A](value: A) extends GenField[A]

object GenField {
  val randomField: GenField[Nothing] = RandomField
  implicit def toSpecifiedField[A](value: A): GenField[A] = SpecifiedField(value)
}
