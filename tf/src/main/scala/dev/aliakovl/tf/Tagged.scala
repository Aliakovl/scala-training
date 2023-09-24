package dev.aliakovl.tf

enum Expr:
  case Plus(left: Expr, right: Expr) extends Expr
  case Mul(left: Expr, right: Expr) extends Expr
  case Const(v: BigInt) extends Expr

import Expr.*
import io.circe._
import io.circe.syntax._

val testExpr =
  Expr.Mul(
    Expr.Plus(
      Expr.Const(1),
      Expr.Const(2)
    ),
    Expr.Plus(
      Expr.Const(3),
      Expr.Const(4)
    )
  )

given Encoder[Expr] with
  def apply(expr: Expr): Json = expr match
    case Plus(left, right) =>
      Map("plus" -> Vector(left.asJson, right.asJson)).asJson
    case Mul(left, right) =>
      Map("mul" -> Vector(left.asJson, right.asJson)).asJson
    case Const(v) => v.asJson

given Decoder[Expr] with
  def apply(c: HCursor): Decoder.Result[Expr] =
    c.value.asNumber
      .flatMap(num => num.toBigInt.map(Const(_)))
      .orElse(
        c.downField("plus")
          .as[Vector[Expr]]
          .toOption
          .collect { case Vector(expr1, expr2) =>
            Plus(expr1, expr2)
          }
      )
      .orElse(
        c.downField("mul")
          .as[Vector[Expr]]
          .toOption
          .collect { case Vector(expr1, expr2) =>
            Mul(expr1, expr2)
          }
      )
      .toRight(DecodingFailure("it's a wrong format, bro", Nil))

@main
def check(): Unit =
  println(testExpr)
  val json = testExpr.asJson
  println(json.spaces2)
  println(json.as[Expr])
