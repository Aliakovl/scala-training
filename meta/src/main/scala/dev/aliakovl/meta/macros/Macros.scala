package dev.aliakovl.meta.macros

import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.*

object Macros:
  inline def blub: String = ${ blubImpl }

  def blubImpl(using Quotes): Expr[String] = '{ "wefqef" }

  inline def length(str: String): Int = ${ lengthImpl('str) }

  def lengthImpl(str: Expr[String])(using Quotes): Expr[Int] =
    '{ $str.length() }

  inline def reify(inline a: Any): String = ${ reifyImpl('a) }

  def reifyImpl(a: Expr[Any])(using quotes: Quotes): Expr[String] = {
    import quotes.reflect.*

    Literal(StringConstant(Printer.TreeStructure.show(a.asTerm)))
      .asExprOf[String]
  }

  def unrolledPowerCode(x: Expr[Double], n: Int)(using Quotes): Expr[Double] =
    if n == 0 then '{ 1.0 }
    else if n == 1 then x
    else '{ $x * ${ unrolledPowerCode(x, n - 1) } }

  def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
    unrolledPowerCode(x, n.valueOrAbort)

  inline def power(x: Double, inline n: Int): Double =
    ${ powerCode('x, 'n) }

  def singletonListExpr[T: Type](x: Expr[T])(using Quotes): Expr[List[T]] =
    '{ List[T]($x) } // generic T used within a quote

  def emptyListExpr[T](using Type[T], Quotes): Expr[List[T]] =
    '{ List.empty[T] } // generic T used within a quote

  def testImpl(using Quotes): Expr[String] = {
    val expr1plus1: Expr[Int] = '{ 1 + 1 }

    reifyImpl(
      expr1plus1
    )

    val expr2: Expr[Int] = Expr(1 + 1) // lift 2 into '{ 2 }

    reifyImpl(
      expr2
    )
  }

  inline def test: String = ${ testImpl }

  def ctx(using Quotes) = {
    def later[T: Type, U: Type](f: Expr[T] => Expr[U]): Expr[T => U] =
      '{ (x: T) => ${ f('x) } }

    def now[T: Type, U: Type](f: Expr[T => U]): Expr[T] => Expr[U] =
      (x: Expr[T]) => Expr.betaReduce('{ $f($x) })

    '{ (x: Int) => x + 1 } match
      case '{ (y: Int) => ${ z }: Int } => z

  }

  def fusedUnrolledPowCode(x: Expr[Double], n: Int)(using
      Quotes
  ): Expr[Double] =
    x match
      case '{ power($y, ${ Expr(m) }) } => // we have (y^m)^n
        fusedUnrolledPowCode(y, n * m) // generate code for y^(n*m)
      case _ =>
        unrolledPowerCode(x, n)

  @tailrec
  private def fuseMapCodeImpl[T: Type](x: Expr[List[T]])(using
      Quotes
  ): Expr[List[T]] =
    x match
      case '{ ($ls: List[t]).map[u]($f).map[T]($g) } =>
        fuseMapCodeImpl[T]('{ ${ ls }.map($g.compose($f)) })
      case t => t

  inline def fuseMapCode[T](inline list: List[T]): List[T] = ${
    fuseMapCodeImpl('list)
  }

  def emptyImpl[T: Type](using Quotes): Expr[T] =
    Type.of[T] match
      case '[String]  => '{ "" }.asExprOf[T]
      case '[List[t]] => '{ List.empty[t] }.asExprOf[T]

  inline def empty[T]: T = ${ emptyImpl[T] }

import scala.annotation.{MacroAnnotation, experimental}

@experimental
class memoize extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case DefDef(name, TermParamClause(param  :: Nil) :: Nil, tpt, Some(rhsTree)) =>
        (param.tpt.tpe.asType, tpt.tpe.asType) match
          case ('[t], '[u]) =>
            val cacheName = Symbol.freshName(name + "Cache")
            val cacheSymbol = Symbol.newVal(Symbol.spliceOwner, cacheName, TypeRepr.of[mutable.Map[t, u]], Flags.Private, Symbol.noSymbol)
            val cacheRhs =
              given Quotes = cacheSymbol.asQuotes
              '{ mutable.Map.empty[t, u] }.asTerm
            val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
            val newRhs =
              given Quotes = tree.symbol.asQuotes
              val cacheRefExpr = Ref(cacheSymbol).asExprOf[mutable.Map[t, u]]
              val paramRefExpr = Ref(param.symbol).asExprOf[t]
              val rhsExpr = rhsTree.asExprOf[u]
              '{ $cacheRefExpr.getOrElseUpdate($paramRefExpr, $rhsExpr) }.asTerm
            val newTree = DefDef.copy(tree)(name, TermParamClause(param :: Nil) :: Nil, tpt, Some(newRhs))
            List(cacheVal, newTree)
      case _ =>
        report.error("Annotation only supported on `def` with a single argument are supported")
        List(tree)
