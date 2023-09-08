package dev.aliakovl.meta.macros

import scala.quoted.{Expr, Quotes}

object Macros:
  inline def blub: String = $ { blubImpl }

  def blubImpl(using Quotes): Expr[String] = '{ "wefqef" }

  inline def length(str: String): Int = ${ lengthImpl('str) }

  def lengthImpl(str: Expr[String])(using Quotes): Expr[Int] =
    '{ $str.length() }
