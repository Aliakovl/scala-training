package dev.aliakovl.dtbrt

import internal.Into
import scala.quoted.*

object internal:
  opaque type Into[From, To] = From

extension[From] (self: From)
  def into[To]: Into[From, To] = ???

extension [From, To](self: Into[From, To])
  def withConstant[Field](selector: To => Field, value: Field): Into[From, To] = ???

extension [From, To](inline self: Into[From, To])
  inline def transform: To = ${ transformImpl[From, To]('self) }

def transformImpl[From: Type, To: Type](expr: Expr[Into[From, To]])(using quotes: Quotes): Expr[To] =
  import quotes.reflect.*

  val (fromExpr, transformations) = extractTransformation(expr)
  report.throwError(s"${fromExpr.show}, \n${transformations}")

  expr match
    case '{ dev.aliakovl.dtbrt.into[From]($fromExpr)[To] } =>
      val fromFieldsMap = getFields[From].map(f => f.name -> f).toMap
      val projections = getFields[To].map { toField =>
        val field = fromFieldsMap.getOrElse(
          toField.name,
          report.throwError(s"I don't know what to do about `${toField.name}`")
        )
        projectField(fromExpr, field)
      }
      construct[To](projections)
    case '{ dev.aliakovl.dtbrt.withConstant[From, To]($fromExpr)[field]($selectorExpr, $valueExpr) } =>
      report.throwError(s"${fromExpr.show}")
//    case qwer => report.throwError(s"${ qwer.show }")
    case _ => report.throwError("OH NO!")

def getFields[A: Type](using quotes: Quotes): List[quotes.reflect.Symbol] =
  import quotes.reflect.*
  TypeRepr.of[A].typeSymbol.caseFields

def projectField[A: Type](using quotes: Quotes)(
  expr: Expr[A],
  fieldSymbol: quotes.reflect.Symbol
): Expr[Any] = {
  import quotes.reflect.*
  Select(expr.asTerm, fieldSymbol).asExprOf[Any]
}

def construct[A: Type](args: List[Expr[Any]])(using quotes: Quotes): Expr[A] =
  import quotes.reflect.*
  val companionSymbol: Symbol = TypeRepr.of[A].typeSymbol.companionModule
  val result = Apply(
    Select.unique(Ref(companionSymbol), "apply"),
    args.map(_.asTerm)
  )
  result.asExprOf[A]

class Transformation[Q <: Quotes](using val quotes: Q)(
  val symbol: quotes.reflect.Symbol,
  val expr: Expr[Any]
)

def extractTransformation[From: Type, To: Type](
  expr: Expr[Into[From, To]]
)(using quotes: Quotes): (Expr[From], List[Transformation[quotes.type]]) =
  import quotes.reflect.*
  def loop(
    expr: Expr[Into[From, To]],
    acc: List[Transformation[quotes.type]]
  ): (Expr[From], List[Transformation[quotes.type]]) = expr match
    case '{ dev.aliakovl.dtbrt.into[From]($fromExpr)[To] } => fromExpr -> acc
    case '{ dev.aliakovl.dtbrt.withConstant[From, To]($nested)[field]($selector, $value) } =>
      val symbol: Symbol = selector.asTerm match
        case Lambda(_, select @ Select(_, _)) => select.symbol
      val transformation = Transformation[quotes.type]()(
        symbol,
        value
      )
      loop(
        nested,
        transformation :: acc
      )

  loop(expr, List.empty)
