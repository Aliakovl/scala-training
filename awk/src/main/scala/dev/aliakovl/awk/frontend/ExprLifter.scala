package dev.aliakovl.awk.frontend

import dev.aliakovl.awk.backend.*

import scala.quoted.{Expr, Quotes}


object ExprLifter:

  def liftMapFilter(
                     awkRootExpr: AwkMapExpr
                   )(using Quotes): Expr[AwkMapExpr] =
    awkRootExpr match {
      case MapExpr(Projection(s: _*)) =>
        val ss = Expr(s)
        '{
          MapExpr(Projection(${ ss }: _*))
        }
    }

  def lift(awkRootExpr: AwkQueryAST)(using Quotes): Expr[AwkQueryAST] =
    awkRootExpr match {
      case AwkQueryAST(fileNameOrPath) =>
        '{ AwkQueryAST(${ Expr(fileNameOrPath) }) }
      case AwkQueryAST(fileNameOrPath, MapExpr(Projection(s: _*))) =>
        val ss = Expr(s)
        '{
          AwkQueryAST(
            ${ Expr(fileNameOrPath) },
            MapExpr(Projection(${ ss }: _*))
          )
        }
      case AwkQueryAST(fileNameOrPath, mexpr: _*) =>
        val res = Expr.ofSeq(mexpr.map(x => liftMapFilter(x)))
        '{
          AwkQueryAST(
            ${ Expr(fileNameOrPath) },
            ${ res }: _*
          )
        }
    }