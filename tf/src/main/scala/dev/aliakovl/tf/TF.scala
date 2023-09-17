package dev.aliakovl.tf

trait ExprA[A]:
  def plus(left: A, right: A): A
  def mul(left: A, right: A): A
  def const(v: BigInt): A

type ExprTF = [A] => ExprA[A] => A

object ExprTF:
  def Plus(left: ExprTF, right: ExprTF): ExprTF =
    [A] => (alg: ExprA[A]) => alg.plus(left(alg), right(alg))

  def Mul(left: ExprTF, right: ExprTF): ExprTF =
    [A] => (alg: ExprA[A]) => alg.mul(left(alg), right(alg))

  def Const(v: BigInt): ExprTF =
    [A] => (alg: ExprA[A]) => alg.const(v)

val testExprTF: ExprTF =
  ExprTFC.Mul(
    ExprTFC.Plus(
      ExprTFC.Const(1),
      ExprTFC.Const(2)
    ),
    ExprTFC.Plus(
      ExprTFC.Const(3),
      ExprTFC.Const(4),
    )
  )

val ShowTF: ExprA[String] = new ExprA[String]:
  override def plus(l: String, r: String): String = s"($l + $r)"
  override def mul(l: String, r: String): String = s"($l * $r)"
  override def const(v: BigInt): String = v.toString()

val CalcTF: ExprA[BigInt] = new {
  override def plus(l: BigInt, r: BigInt): BigInt = l + r
  override def mul(l: BigInt, r: BigInt): BigInt = l * r
  override def const(v: BigInt): BigInt = v
}

trait Vars[A]:
  def variable(name: String): A

trait Matrices[A]:
  def concat(x: A, y: A): A

type ExprVTF = [A] => (ExprA[A], Vars[A]) => A
type ExprMTF = [A] => (ExprA[A], Matrices[A]) => A
type ExprVMTF = [A] => (ExprA[A], Matrices[A], Vars[A]) => A

def enhanceWithVariables(e: ExprTF): ExprVTF =
  [A] => (alg: ExprA[A], vars: Vars[A]) => e(alg)

def superEnhance(expr: ExprTF): ExprVMTF =
  [A] => (alg: ExprA[A], m: Matrices[A], vars: Vars[A]) => expr(alg)

@main
def checkTF(): Unit =
  println(testExprTF(ShowTF))
  println(testExprTF(CalcTF))