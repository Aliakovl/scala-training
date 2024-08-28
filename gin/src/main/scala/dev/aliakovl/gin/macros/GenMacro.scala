package dev.aliakovl.gin.macros

import dev.aliakovl.gin.{Gen, GenOps, Random}

import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  def randomImpl[A: c.WeakTypeTag](gen: c.Expr[Gen[A]]): c.Expr[GenOps[A]] = {
    val genTree = disassembleTree(gen)

    val mo: OpticsMerge = mergeOptics(genTree)

    val sub = subclassesOf(genTree.genClass)

    val f = q"${c.inferImplicitValue(typeOf[Random[Int]])}.get()"

    val t = findUnique(mo)
      .map(s => {
        val ss = s.info.baseType(s.info.typeSymbol)
        val t = appliedType(typeOf[Random[_]].typeConstructor, ss)
        (s, ss, s.info.typeArgs, s.isJava, t, c.inferImplicitValue(t))
      })
      .mkString("\n")

    val c1 = Select(
      New(Ident(sub.find(_.fullName == "dev.aliakovl.gin.MyClass1").get)),
      termNames.CONSTRUCTOR
    )

    c.Expr[GenOps[A]](
      q"""new _root_.dev.aliakovl.gin.GenOps[${c.weakTypeOf[A]}] {
        override def random = {
          println(debug)
          def f = ${f}
          println(f)
          println(f)
          _root_.dev.aliakovl.gin.Random($c1(implicitly[Random[MyClass2]].get()))
        }
        override def debug = ${mkStr(
          showRaw(f) +: t +:
            mo.toString +:
            genTree.toString +: sub.toList.map { cl =>
              showRaw(cl) + {
                if (!cl.isModuleClass)
                  publicConstructor(cl.asClass).paramLists
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

  def publicConstructor(parent: ClassSymbol): MethodSymbol = {
    val members = parent.info.members
    members
      .find(m => m.isMethod && m.asMethod.isPrimaryConstructor && m.isPublic)
      .orElse(
        members.find(m => m.isMethod && m.asMethod.isConstructor && m.isPublic)
      )
      .get
      .asMethod
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

  sealed trait OpticsMerge
  case class ProductMerge(fields: Map[c.Symbol, OpticsMerge])
      extends OpticsMerge
  case class CoproductMerge(subclasses: Map[c.Symbol, OpticsMerge])
      extends OpticsMerge
  case class ApplyOptic(tree: c.Tree) extends OpticsMerge
  case object ONil extends OpticsMerge

  def mergeOptics(genTree: GenTree): OpticsMerge = {
    val GenTree(genClass, specs) = genTree

    def help(
        classSymbol: ClassSymbol,
        selector: List[Optic],
        tree: c.Tree
    ): OpticsMerge = {
      selector match {
        case Prism(_, to) :: tail =>
          val subs = subclassesOf(to.asClass) + to
          CoproductMerge(subclasses = subclassesOf(classSymbol).map {
            subclass =>
              if (subs.contains(subclass)) {
                subclass -> help(to.asClass, tail, tree)
              } else {
                subclass -> ONil
              }
          }.toMap)
        case Lens(tn) :: tail =>
          ProductMerge(fields =
            publicConstructor(classSymbol).paramLists.flatten.map { param =>
              if (param.asTerm.name == tn) {
                param -> help(
                  param.info.baseClasses.head.asClass,
                  tail,
                  tree
                )
              } else {
                param -> ONil
              }
            }.toMap
          )
        case Nil => ApplyOptic(tree)
      }
    }

    specs
      .map { case (optics, tree) => help(genClass, optics, tree) }
      .foldLeft(ONil: OpticsMerge)(mergeOptics)
  }

  def mergeOptics(left: OpticsMerge, right: OpticsMerge): OpticsMerge = {
    (left, right) match {
      case (ProductMerge(l), ProductMerge(r)) =>
        ProductMerge(l.map { case (k, v) =>
          k -> mergeOptics(v, r.getOrElse(k, ONil))
        })
      case (CoproductMerge(l), CoproductMerge(r)) =>
        CoproductMerge(l.map { case (k, v) =>
          k -> mergeOptics(v, r.getOrElse(k, ONil))
        })
      case (CoproductMerge(l), pm: ProductMerge) =>
        CoproductMerge(subclasses = l.map { case (subclass, ls) =>
          subclass -> mergeOptics(ls, pm)
        })
      case (lm: ProductMerge, CoproductMerge(r)) =>
        CoproductMerge(subclasses = r.map { case (subclass, rs) =>
          subclass -> mergeOptics(lm, rs)
        })
      case (m @ ProductMerge(fields), ONil) => if (fields.nonEmpty) m else ONil
      case (m @ CoproductMerge(subclasses), ONil) =>
        if (subclasses.nonEmpty) m else ONil
      case (ONil, m @ ProductMerge(fields)) => if (fields.nonEmpty) m else ONil
      case (ONil, m @ CoproductMerge(subclasses)) =>
        if (subclasses.nonEmpty) m else ONil
      case (ONil, ONil) => ONil
      case (_, _: ApplyOptic) | (_: ApplyOptic, _) =>
        c.abort(
          c.enclosingPosition,
          s"double application leads to erasure"
        )
    }
  }

  def findUnique(opticsMerge: OpticsMerge): Set[c.Symbol] = {
    opticsMerge match {
      case ProductMerge(fields) =>
        val (leaves, branches) = fields.partition(_._2 == ONil)
        leaves.keys.toSet union branches.values
          .flatMap(findUnique)
          .toSet
      case CoproductMerge(subclasses) =>
        val (leaves, branches) = subclasses.partition(_._2 == ONil)
        leaves.keySet union branches.values.flatMap(findUnique).toSet
      case _ => Set.empty
    }
  }
}
