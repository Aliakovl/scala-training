package dev.aliakovl.gin.macros

import dev.aliakovl.gin.Random

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class RandomTransformerImpl(val c: whitebox.Context) {
  import c.universe._

  def common[A: c.WeakTypeTag, U: c.WeakTypeTag](
      selector: c.Expr[A => U],
      ra: c.Expr[Random[A]],
      ru: c.Expr[Random[U]]
  ): (c.Expr[Random[A]], c.Expr[String]) = {
    val (t1, t2) = selector.tree match {
      case q"(..$arg) => $pathBody" => (arg, pathBody)
    }

    val r =
      c.weakTypeOf[A].member(TermName("<init>")).asMethod.paramLists.mkString

    //    symbolOf[A].

    val g = c.untypecheck(ra.tree.duplicate)

    implicit val la: Liftable[A] = {
      new Liftable[A] {
        override def apply(value: A): c.universe.Tree = {
          val init = c.weakTypeOf[A].member(TermName("<init>"))

          val args = init.asMethod.paramLists.map(_.map { s =>
            q"${s.name.toTermName} = ${value.toString}.${s.name.toTermName}"
          })

          q"${init.asMethod}(...$args)"
        }
      }
    }

    implicit val lra: Liftable[Random[A]] = {
      new Liftable[Random[A]] {
        override def apply(value: Random[A]): c.universe.Tree = {
          implicitly[Liftable[A]]
          q"""_root_.dev.aliakovl.gin.Random(${value.get()})"""
        }
      }
    }

    val werf =
      q"${c.eval[Random[A]](c.Expr[Random[A]](c.untypecheck(ra.tree.duplicate)))}"

    val str = mkStr(
      //      showRaw(t1),
      //      showRaw(t2),
      //      r,
      //      showRaw(ru.tree),
      showRaw(ra.tree),
      showCode(werf)
    )

    (ra, str)
  }

  def specify[A: c.WeakTypeTag, U: c.WeakTypeTag](
      selector: c.Expr[A => U],
      ra: c.Expr[Random[A]],
      ru: c.Expr[Random[U]]
  ): c.Expr[Random[A]] = common(selector, ra, ru)._1

  def help[A: c.WeakTypeTag, U: c.WeakTypeTag](
      selector: c.Expr[A => U],
      ra: c.Expr[Random[A]],
      ru: c.Expr[Random[U]]
  ): c.Expr[String] = common(selector, ra, ru)._2

  private def mkStr(str: String*): c.Expr[String] = c.Expr[String](
    Literal(
      Constant(
        str.mkString("\n")
      )
    )
  )

}

object RandomTransformer {
  def specify[A, U](
      selector: A => U,
      ra: Random[A],
      ru: Random[U]
  ): Random[A] = macro RandomTransformerImpl.specify[A, U]

  def help[A, U](
      selector: A => U,
      ra: Random[A],
      ru: Random[U]
  ): String = macro RandomTransformerImpl.help[A, U]
}
