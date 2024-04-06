package dev.aliakovl.gin

import zio.ZIO

trait Sex
case object Male extends Sex
case object Female extends Sex

case class User(age: Int, name: String, sex: Option[Sex])

object Main {
  type F[A] = ZIO[Any, Nothing, A]

//  val a: Gen[User] = Gen[User]
//    .specify(_.name, "wef")
//    .specify(_.sex, Some(Male))
//    .generate[F]()

  given Interpreter[F, User] = ???

  implicit val a: String = "ef"
  implicit val b: Int = 2

  foo.addimpl

  def main(args: Array[String]): Unit = {

  }
}
