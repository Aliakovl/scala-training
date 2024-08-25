package dev.aliakovl.gin.macros

import dev.aliakovl.gin.{Gen, GenOps}

import scala.jdk.Accumulator
import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  def randomImpl[A: c.WeakTypeTag](gen: c.Expr[Gen[A]]): c.Expr[GenOps[A]] = {
    val other = go(gen.tree, List.empty)

    val tr = other._2.symbol.asClass
    val sub = subclassesOf(tr)

    val c1 = Select(
      New(Ident(sub.find(_.fullName == "dev.aliakovl.gin.MyClass1").get)),
      termNames.CONSTRUCTOR
    )







    c.Expr[GenOps[A]](
      q"""new _root_.dev.aliakovl.gin.GenOps[${c.weakTypeOf[A]}] {
        override def random = {
          println(debug)
          _root_.dev.aliakovl.gin.Random(${c1}(implicitly[Random[MyClass2]].get()))
        }
        override def debug = ${mkStr(
        f(other._1.head.head) +: showRaw(tr) +: subclassesOf(tr).toList.map { cl =>
            showRaw(cl) + {
              if (!cl.isModuleClass)
                publicConstructors(cl.asClass)
                  .map(_.map(_.info))
                  .map(_.map(showRaw(_)))
                  .map(_.mkString("(", ",", ")"))
                  .mkString("(", ",", ")")
              else ""
            }
          } ++: other._1
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
      case q"$other.specify[..$_](($_) => $selector, $random)" => go(other, List(selector, random) +: acc)
      case q"$expr[$tpts]"                  => (acc, tpts)
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

    concreteChildren ++ abstractChildren.flatMap { child =>
      val childClass = child.asClass
      if (childClass.isSealed) {
        subclassesOf(childClass)
      } else {
        c.abort(c.enclosingPosition, s"child $child of $parent is not sealed")
      }
    }
  }

  private def publicConstructors(parent: ClassSymbol): List[List[Symbol]] = {
    val members = parent.info.members
    members
      .find(m => m.isMethod && m.asMethod.isPrimaryConstructor && m.isPublic)
      .orElse(
        members.find(m => m.isMethod && m.asMethod.isConstructor && m.isPublic)
      )
      .get
      .asMethod
      .paramLists
  }

  private def f(selector: Tree): String = {
    Accumulator.unfold(selector) {
      case q"$other.${field: TermName}" => Some(Lens(field), other)
      case q"""$module[$from]($other).when[$to]""" => Some(Prism(from.symbol, to.symbol), other)
      case _ => None
    }.map(showRaw(_)).mkString(",")
  }

  trait Optic
  case class Lens(tn: TermName) extends Optic
  case class Prism(from: Symbol, to: Symbol) extends Optic

  case class GenTree(genClass: ClassSymbol, specs: List[(Optic, Tree)])
}
