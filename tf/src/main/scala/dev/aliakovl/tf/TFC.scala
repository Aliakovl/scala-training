package dev.aliakovl.tf

import io.circe.*
import io.circe.syntax.*

type ExprTFC = [A] => ExprA[A] ?=> A
type ExprVTFC = [A] => (ExprA[A], Vars[A]) ?=> A
type ExprMTFC = [A] => (ExprA[A], Matrices[A]) ?=> A
type ExprVMTFC = [A] => (ExprA[A], Matrices[A], Vars[A]) ?=> A

type ExprLang[A] = ExprA[A] ?=> A
type VarLang[A] = Vars[A] ?=> A
type MatLang[A] = Matrices[A] ?=> A
type ExprLangV[A] = (ExprA[A], Vars[A]) ?=> A
type ExprLangM[A] = (ExprA[A], Matrices[A]) ?=> A
type ExprLangVM[A] = (ExprA[A], Matrices[A], Vars[A]) ?=> A

object ExprTFC:
  def Plus(left: ExprTFC, right: ExprTFC): ExprTFC =
    [A] => (alg: ExprA[A]) ?=> alg.plus(left[A], right[A])

  def Mul(left: ExprTFC, right: ExprTFC): ExprTFC =
    [A] => (alg: ExprA[A]) ?=> alg.mul(left[A], right[A])

  def Const(v: BigInt): ExprTFC =
    [A] => (alg: ExprA[A]) ?=> alg.const(v)

val testExprTFC: ExprTFC =
  ExprTFC.Mul(
    ExprTFC.Plus(
      ExprTFC.Const(1),
      ExprTFC.Const(2)
    ),
    ExprTFC.Plus(
      ExprTFC.Const(3),
      ExprTFC.Const(4)
    )
  )

object ExprLang:
  def Plus[A](left: A, right: A): ExprLang[A] =
    (alg: ExprA[A]) ?=> alg.plus(left, right)

  def Mul[A](left: A, right: A): ExprLang[A] =
    (alg: ExprA[A]) ?=> alg.mul(left, right)

  def Const[A](v: BigInt): ExprLang[A] =
    (alg: ExprA[A]) ?=> alg.const(v)

object VarLang:
  def Var[A](name: String): VarLang[A] =
    alg ?=> alg.variable(name)

object MatLang:
  def Concat[A](l: A, r: A): MatLang[A] =
    alg ?=> alg.concat(l, r)

def testExprLang[A]: ExprLang[A] =
  ExprLang.Mul(
    ExprLang.Plus(
      ExprLang.Const(1),
      ExprLang.Const(2)
    ),
    ExprLang.Plus(
      ExprLang.Const(3),
      ExprLang.Const(4)
    )
  )

def testExpr2[A]: ExprLangV[A] =
  ExprLang.Mul(
    ExprLang.Plus(VarLang.Var("x"), ExprLang.Const(2)),
    VarLang.Var("y")
  )

def testExpr2M[A]: ExprLangVM[A] = testExpr2

given ExprA[String] with
  def plus(l: String, r: String) = s"($l + $r)"
  def mul(l: String, r: String) = s"($l * $r)"
  def const(v: BigInt): String = v.toString

given ExprA[BigInt] with
  def plus(l: BigInt, r: BigInt): BigInt = l + r
  def mul(l: BigInt, r: BigInt): BigInt = l * r
  def const(v: BigInt): BigInt = v

given ExprA[Json] with
  def plus(l: Json, r: Json) = Map("plus" -> Vector(l.asJson, r.asJson)).asJson
  def mul(l: Json, r: Json) = Map("mul" -> Vector(l.asJson, r.asJson)).asJson
  def const(v: BigInt) = v.asJson

given Encoder[ExprTFC] with
  def apply(expr: ExprTFC) = expr[Json]

import ExprTFC.*

given Decoder[ExprTFC] with
  def apply(c: HCursor): Decoder.Result[ExprTFC] =
    c.value.asNumber
      .flatMap(num => num.toBigInt.map(Const(_)))
      .orElse(
        c.downField("plus")
          .as[Vector[ExprTFC]]
          .toOption
          .collect { case Vector(expr1, expr2) =>
            Plus(expr1, expr2)
          }
      )
      .orElse(
        c.downField("mul")
          .as[Vector[ExprTFC]]
          .toOption
          .collect { case Vector(expr1, expr2) =>
            Mul(expr1, expr2)
          }
      )
      .toRight(DecodingFailure("it's a wrong format, bro", Nil))

def enhanceWithVariablesC(e: ExprTFC): ExprVTFC =
  [A] => (alg: ExprA[A], vars: Vars[A]) ?=> e[A]

@main
def checkTFC(): Unit =
  println(testExprTFC[String])
  println(testExprTFC[BigInt])
  val json = testExprTFC.asJson
  println(json.spaces2)
  val parsed = json.as[ExprTFC]
  println(parsed.map(_[String]))
