package dev.aliakovl.gin.macros

import scala.reflect.macros.whitebox

class GenMacro(val c: whitebox.Context) {
  import c.universe._

  def randomImpl[A: c.WeakTypeTag](gen: Tree): c.Expr[GenOps[A]] = {
    mkGenOps[A](mkStr(showRaw(gen)))
  }

  def mkGenOps[A: c.WeakTypeTag](random: String): c.Expr[GenOps[A]] = {
    c.Expr[GenOps[A]](q"""_root_.dev.aliakovl.gin.macros.GenOps[${c.weakTypeOf[A]}](random = $random)""")
  }

  private def mkStr(str: String*): String = str.mkString("\n")
}
