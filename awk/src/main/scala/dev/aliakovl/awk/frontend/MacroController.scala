package dev.aliakovl.awk.frontend

import dev.aliakovl.awk.backend.*

import scala.quoted.{Expr, Quotes, Type}

case class Quoted[+T](expr: AwkQueryAST)

object Quoted:

  def apply[T](awkRootExpr: AwkQueryAST) = new Quoted[T](awkRootExpr)

  inline def quote[T](inline bodyExpr: T): Quoted[T] =
    ${ MacroController.process[T]('bodyExpr) }

object MacroController:

  def process[T](
      bodyRaw: Expr[T]
  )(using quotes: Quotes, t: Type[T]): Expr[Quoted[T]] =
    import quotes.reflect.*
    val ast = ExprParser.parseQuery(bodyRaw)
    import quotes.reflect.report
    val normalized = ASTNormalizer.normalize(ast)
    report.info(ASTCompiler.compile(normalized))
    val liftedQuery = ExprLifter.lift(normalized)
    '{ Quoted[T](${ liftedQuery }) }
