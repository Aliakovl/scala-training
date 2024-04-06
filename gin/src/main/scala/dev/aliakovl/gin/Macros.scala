package dev.aliakovl.gin

import scala.quoted.*

import dev.aliakovl.gin.GenField.*

enum GenField[+A] {
  case RandomField
  case SpecifiedField(value: A)
}

class Gen[A]

object Gen {
  inline def apply[A]: Gen[A] = ${ applyImpl[A] }

  private def applyImpl[A: Type](using quotes: Quotes): Expr[Gen[A]] = ???

  private def getFields[A: Type](using
      quotes: Quotes
  ): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    import quotes.*
    TypeRepr.of[A].typeSymbol.caseFields
  }
}

extension [A](self: Gen[A]) {
  def specify[Field](selector: A => Field, value: Field): Gen[A] = ???
}

trait Generator[F[_], A] {
  def generate: F[A]
}

trait Interpreter[F[_], A] {
  def generate(gen: Gen[A]): F[A]
}

trait Randomizer[A] {
  def random: A
}

extension [A](inline self: Gen[A]) {
  inline def generate[F[_]](using int: Interpreter[F, A]): F[A] = ${
    generateImpl[F, A]('self)
  }
}

def generateImpl[F[_]: Type, A: Type](
    expr: Expr[Gen[A]]
)(using quotes: Quotes): Expr[F[A]] = {
  import quotes.reflect.*
  import quotes.*

  report.error(expr.show)

  expr match
    case _ => ???

}

extension [T](inline f: T) {
  inline def addimpl: T = ${ addimplImpl[T]('{ f }) }
}

inline def foo(implicit i: Int, s: String): Unit = ()

def addimplImpl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[T] = {
  import quotes.reflect.*

  report.info(s"${expr.asTerm}", expr)

  expr
}
