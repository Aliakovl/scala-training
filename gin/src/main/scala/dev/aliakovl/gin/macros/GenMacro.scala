package dev.aliakovl.gin.macros

import dev.aliakovl.gin.{Gen, GenOps}

import scala.reflect.macros.whitebox

class GenMacro(val c: whitebox.Context) {
  import c.universe._

  def randomImpl[A: c.WeakTypeTag](gen: c.Expr[Gen[A]]): c.Expr[GenOps[A]] = {

    val other = go(gen.tree, List.empty)

//    val t = other._2.symbol.asClass.primaryConstructor.asMethod.paramLists
//    val ttype = other._2.symbol.asClass.primaryConstructor.asMethod

    val tr = other._2.symbol.asClass
    val sub = subclassesOf(tr)

    val c1 = Select(
      New(Ident(sub.find(_.fullName == "dev.aliakovl.gin.MyClass1").get)),
      termNames.CONSTRUCTOR
    )

//    mkGenOps[A](c.Expr[A](t), )

    c.Expr[GenOps[A]](
      q"""new _root_.dev.aliakovl.gin.GenOps[${c.weakTypeOf[A]}] {
        override def random = {
          println(debug)
          _root_.dev.aliakovl.gin.Random(${c1}(implicitly[Random[MyClass2]].get()))
        }
        override def debug = ${mkStr(
          showRaw(tr) +: subclassesOf(tr).toList.map{cl => showRaw(cl) + {
            if (!cl.isModuleClass) publicConstructors(cl.asClass).map(_.map(showRaw(_))).map(_.mkString("(", ",", ")")).mkString("(", ",", ")") else ""
          } } ++: other._1
            .map(
              showRaw(_)
            ): _*
        )}
      }"""
    )
  }

//  def mkGenOps[A: c.WeakTypeTag](random: c.Expr[A], debug: String): c.Expr[GenOps[A]] = {
//    c.Expr[GenOps[A]](q"""new _root_.dev.aliakovl.gin.GenOps[${c.weakTypeOf[A]}] {
//                            override val random = _root_.dev.aliakovl.gin.Random(() => ${random.splice})
//                            override val debug = $debug
//                          }""")
//  }

  private def mkStr(str: String*): String = str.mkString("\n")

  private def go(gen: Tree, acc: List[List[Tree]]): (List[List[Tree]], Tree) = {
    gen match {
      case q"$other.specify[..$_](..$exprss)" => go(other, exprss +: acc)
      case q"$expr[..$tpts]"                  => (acc, tpts.head)
    }
  }

  private def subclassesOf(parent: ClassSymbol): Set[Symbol] = {
    val (abstractChildren, concreteChildren) =
      parent.knownDirectSubclasses.partition(_.isAbstract)

    concreteChildren.foreach { child =>
      if (!child.isFinal && !child.asClass.isCaseClass) {
        c.abort(
          c.enclosingPosition,
          s"child $child of $parent is neither final nor a case class"
        )
      }
    }

    concreteChildren.union {
      abstractChildren.flatMap { child =>
        val childClass = child.asClass
        if (childClass.isSealed) {
          subclassesOf(childClass)
        } else {
          c.abort(c.enclosingPosition, s"child $child of $parent is not sealed")
        }
      }

    }
  }

  def publicConstructors(parent: ClassSymbol): List[List[Symbol]] = {
    val members = parent.info.members
    val c = members
      .find(m => m.isMethod && m.asMethod.isPrimaryConstructor && m.isPublic)
      .orElse(
        members.find(m => m.isMethod && m.asMethod.isConstructor && m.isPublic)
      )
      .get

    c.asMethod.paramLists
  }

}
