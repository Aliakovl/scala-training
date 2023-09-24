package dev.aliakovl.dtbrt

import internal.Into

import scala.annotation.tailrec
import scala.quoted.*

object internal:
  opaque type Into[From, To] = From

extension[From] (self: From)
  def into[To]: Into[From, To] = ???

extension [From, To](self: Into[From, To])
  def withConstant[Field](selector: To => Field, value: Field): Into[From, To] = ???
  def withComputed[Field](selector: To => Field, f: From => Field): Into[From, To] = ???

extension [From, To](inline self: Into[From, To])
  inline def transform: To = ${ transformImpl[From, To]('self) }

def transformImpl[From: Type, To: Type](expr: Expr[Into[From, To]])(using quotes: Quotes): Expr[To] =
  import quotes.reflect.*
  val (fromExpr, transformations) = extractTransformation(expr)
  val fromFieldsMap = getFields[From].map(f => f.name -> f).toMap
  val toFields = getFields[To]
  val transformationMap =
    transformations.map(t => t.symbol.name -> t.makeExpr(fromExpr)).toMap
  val args = toFields.map { field =>
    transformationMap.getOrElse(field.name, makeFallback(fromExpr, field, fromFieldsMap))
  }
  construct[To](args)

def makeFallback[From: Type](using quotes: Quotes)(
    fromExpr: Expr[From],
    toField: quotes.reflect.Symbol,
    fromFieldMap: Map[String, quotes.reflect.Symbol]
): Expr[Any] =
  import quotes.reflect.*
  val field = fromFieldMap.getOrElse(
    toField.name,
    report.throwError(s"I don't know what to do about `${toField.name}`")
  )
  projectField(fromExpr, field)


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

class Transformation[From: Type, Q <: Quotes](using val quotes: Q)(
  val symbol: quotes.reflect.Symbol,
  val makeExpr: Expr[From] => Expr[Any]
):
  override def toString: String = s"Transformation(${symbol.name}, ${makeExpr})"

def extractTransformation[From: Type, To: Type](
  expr: Expr[Into[From, To]]
)(using quotes: Quotes): (Expr[From], List[Transformation[From, quotes.type]]) =
  import quotes.reflect.*
  @tailrec
  def loop(
    expr: Expr[Into[From, To]],
    acc: List[Transformation[From, quotes.type]]
  ): (Expr[From], List[Transformation[From, quotes.type]]) = expr match
    case '{ dev.aliakovl.dtbrt.into[From]($fromExpr)[To] } => fromExpr -> acc
    case '{ dev.aliakovl.dtbrt.withConstant[From, To]($nested)[field]($selector, $value) } =>
      val symbol: Symbol = selector.asTerm match
        case Lambda(_, select @ Select(_, _)) => select.symbol
        case term => report.throwError("only projection is available", term.pos)
      val transformation = Transformation[From, quotes.type](symbol, _ => value)
      loop(nested, transformation :: acc)
    case '{ dev.aliakovl.dtbrt.withComputed[From, To]($nested)[field]($selector, $f) } =>
      val symbol: Symbol = selector.asTerm match
        case Lambda(_, select@Select(_, _)) => select.symbol
        case term => report.throwError("only projection is available", term.pos)
      val transformation = Transformation[From, quotes.type](
        symbol,
        fromExpr => Expr.betaReduce('{ $f($fromExpr) })
      )
      loop(nested, transformation :: acc)
    case _ => report.throwError("error")
  loop(expr, List.empty)
