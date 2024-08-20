package dev.aliakovl.gin.macros

import scala.reflect.macros.whitebox

class GenMacro(val c: whitebox.Context) {
  import c.universe._

  def randomImpl[A: c.WeakTypeTag](gen: Tree): c.Expr[GenOps[A]] = {

    val other = go(gen, List.empty)

    val t = other._2.symbol.asClass.primaryConstructor.asMethod.paramLists
    val ttype = other._2.symbol.asClass.primaryConstructor.asMethod

    val c1 = Select(New(Ident(other._2.symbol)), termNames.CONSTRUCTOR)

    val tr = other._2.symbol.asClass

//    mkGenOps[A](c.Expr[A](t), )

    c.Expr[GenOps[A]](q"""new _root_.dev.aliakovl.gin.macros.GenOps[${c.weakTypeOf[A]}] {
        override def random = {
          println(debug)
          _root_.dev.aliakovl.gin.Random(${c1}(implicitly[Random[MyClass2]].get()))
        }
        override def debug = ${mkStr(showRaw(tr) +: other._1.map(showRaw(_)): _*)}
      }""")
  }

//  def mkGenOps[A: c.WeakTypeTag](random: c.Expr[A], debug: String): c.Expr[GenOps[A]] = {
//    c.Expr[GenOps[A]](q"""new _root_.dev.aliakovl.gin.macros.GenOps[${c.weakTypeOf[A]}] {
//                            override val random = _root_.dev.aliakovl.gin.Random(() => ${random.splice})
//                            override val debug = $debug
//                          }""")
//  }

  private def mkStr(str: String*): String = str.mkString("\n")

  private def go(gen: Tree, acc: List[List[Tree]]): (List[List[Tree]], Tree) = {
    gen match {
      case q"$other.specify[..$_](..$exprss)" => go(other, exprss +: acc)
      case q"$expr[..$tpts]" => (acc, tpts.head)
    }
  }
}
