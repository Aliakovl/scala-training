package dev.aliakovl.gin

import shapeless._

sealed trait Sex
case object Male extends Sex
case object Female extends Sex

case class User(age: Int, name: String, sex: Option[Sex])

object Main {
  def main(args: Array[String]): Unit = {
    type A = GenRule[
      GenRule[Int] :: GenRule[String] :: GenRule[Option[GenRule[Male.type :+: Female.type :+: CNil]]] :: HNil
    ]

    println(Random[A].apply())
  }
}
