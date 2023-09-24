package dev.aliakovl.awk.backend

import scala.annotation.tailrec

object ASTNormalizer:
  private def expandComposite(exprs: List[AwkMapExpr]): List[AwkMapExpr] =
    exprs match
      case Nil => Nil
      case head :: tail =>
        head match
          case CompositeMap(innerExpr) => innerExpr ::: exprs
          case _                       => head :: expandComposite(tail)

  private def mergeProjections(
      exprs: List[AwkMapExpr]
  ): List[AwkMapExpr] = exprs match
    case MapExpr(left) :: MapExpr(right) :: tail =>
      val m = Map.from(left.idx.zipWithIndex.map { (x, index) =>
        (index + 1) -> x
      })
      MapExpr(
        Projection(right.idx.map(prj => m(prj)): _*)
      ) :: tail
    case head :: tail => head :: mergeProjections(tail)
    case Nil          => Nil

  @tailrec
  private def normalizeList(
      exprs: List[AwkMapExpr]
  ): List[AwkMapExpr] =
    val result = expandComposite.andThen(mergeProjections)(exprs)
    if result != exprs then normalizeList(result)
    else result

  def normalize(awkRootExpr: AwkQueryAST): AwkQueryAST = AwkQueryAST(
    awkRootExpr.fileNameOrPath,
    normalizeList(awkRootExpr.mapFilterExpr.toList): _*
  )
