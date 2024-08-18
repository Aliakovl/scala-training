package dev.aliakovl.gin.macros

import dev.aliakovl.gin.Random

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class RandomTransformerImpl(val c: whitebox.Context) {
  def specify[A: c.WeakTypeTag, U: c.WeakTypeTag](
      selector: c.Expr[A => U],
      ra: c.Expr[Random[A]],
      ru: c.Expr[Random[U]]
  ): c.Expr[Random[A]] = {
    import c.universe._

    ra
  }

  def help[A: c.WeakTypeTag, U: c.WeakTypeTag](
      selector: c.Expr[A => U],
      ra: c.Expr[Random[A]],
      ru: c.Expr[Random[U]]
  ): c.Expr[String] = {
    import c.universe._

    val a: Tree = q""" val e = "qwefqwef" """

    val q"val e = $value" = a

//    c.Expr[String](Literal(Constant(value)))

//    c.Expr[String](value)

    val (t1, t2) = selector.tree match {
      case q"(..$arg) => $pathBody" => (arg, pathBody)
    }

//    val q"${name: TermName}" = ra

    implicit def lra[T: Liftable]: Liftable[Random[T]] = new Liftable[Random[T]] {
      override def apply(value: Random[T]): c.universe.Tree = {
        q"_root_.dev.aliakovl.gin.Random(null)"
      }
    }

    val r = c.weakTypeOf[A].member(TermName("<init>")).asMethod

//    val werf = q"${c.eval[Random[A]](c.Expr[Random[A]](c.untypecheck(ra.tree.duplicate)))}"

    c.Expr[String](
      Literal(
        Constant(
          showRaw(t1) ++ "\n" ++
            showRaw(t2) ++ "\n" ++
            r.fullName ++ "\n" ++
            showRaw(ru.tree)
        )
      )
    )
  }
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
