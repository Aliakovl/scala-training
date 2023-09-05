package dev.aliakovl.meta.macros

import scala.quoted.{Expr, Quotes}

object Macros {

  inline def blub: Unit = ${ blubImpl }

  def blubImpl(using Quotes): Expr[Unit] = '{ () }

  inline def length(str: String): Int = ${ lengthImpl('str) }

  def lengthImpl(str: Expr[String])(using Quotes): Expr[Int] =
    '{ $str.length() }

}
