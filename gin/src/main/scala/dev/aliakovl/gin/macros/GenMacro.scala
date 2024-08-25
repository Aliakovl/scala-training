package dev.aliakovl.gin.macros

import dev.aliakovl.gin.{Gen, GenOps}

import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  def randomImpl[A: c.WeakTypeTag](gen: c.Expr[Gen[A]]): c.Expr[GenOps[A]] = {
    val genTree = disassembleTree(gen)

    val sub = subclassesOf(genTree.genClass)

    val c1 = Select(
      New(Ident(sub.find(_.fullName == "dev.aliakovl.gin.MyClass1").get)),
      termNames.CONSTRUCTOR
    )

    c.Expr[GenOps[A]](
      q"""new _root_.dev.aliakovl.gin.GenOps[${c.weakTypeOf[A]}] {
        override def random = {
          println(debug)
          _root_.dev.aliakovl.gin.Random($c1(implicitly[Random[MyClass2]].get()))
        }
        override def debug = ${mkStr(
          genTree.toString +: sub.toList.map { cl =>
            showRaw(cl) + {
              if (!cl.isModuleClass)
                publicConstructors(cl.asClass)
                  .map(_.map(_.info))
                  .map(_.map(showRaw(_)))
                  .map(_.mkString("(", ",", ")"))
                  .mkString("(", ",", ")")
              else ""
            }
          }: _*
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

  def mkStr(str: String*): String = str.mkString("\n")

  def subclassesOf(parent: ClassSymbol): Set[Symbol] = {
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

  def publicConstructors(parent: ClassSymbol): List[List[Symbol]] = {
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

  sealed trait Optic
  case class Lens(tn: c.TermName) extends Optic
  case class Prism(from: c.Symbol, to: c.Symbol) extends Optic

  case class GenTree(genClass: ClassSymbol, specs: List[(List[Optic], Tree)]) {
    override def toString: String =
      s"""GenTree(
        |  genClass = ${showRaw(genClass)},
        |  specs = ${specs.map(_._1).mkString("(\n    ", ",\n    ", "\n  )")}
        |)""".stripMargin
  }

  def disassembleTree[A: WeakTypeTag](tree: c.Expr[Gen[A]]): GenTree = {
    val genClass = symbolOf[A].asClass
    val specs: List[(List[Optic], c.Tree)] = List
      .unfold(tree.tree) {
        case q"$other.specify[..$_](($_) => $selector, $random)" =>
          Some((disassembleSelector(selector).reverse, random), other)
        case q"$_[$_]" => None
      }
      .reverse
    GenTree(genClass, specs)
  }

  def disassembleSelector(selector: c.Tree): List[Optic] = {
    List.unfold(selector) {
      case q"$other.${field: TermName}" => Some(Lens(field), other)
      case q"""$module[$from]($other).when[$to]""" =>
        Some(Prism(from.symbol, to.symbol), other)
      case _ => None
    }
  }
}
