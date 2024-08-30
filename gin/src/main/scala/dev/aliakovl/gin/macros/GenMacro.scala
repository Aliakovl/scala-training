package dev.aliakovl.gin.macros

import dev.aliakovl.gin.{Gen, GenOps, Random}

import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  def randomImpl[A: c.WeakTypeTag](gen: c.Expr[Gen[A]]): c.Expr[GenOps[A]] = {
    val mo = mergeOptics(disassembleTree(gen))
    val resTree = mo.map(mkTree).map(toRandom) // mkTree нужно делать уже имея в контекте часть необходимых переменные, а не втавлять implicitly[...]
    val r = mo
      .map(findUnique)
      .map { types =>
        resTree.foreach(resultTree =>
          createDefs(types, resultTree, weakTypeOf[A])
        )
      }

    mkGenOps[A](Some(q"null").map(toRandom), r.map(show(_)).getOrElse(""))
  }

  def mkGenOps[A: c.WeakTypeTag](
      random: Option[c.Tree],
      debug: String
  ): c.Expr[GenOps[A]] = {
    c.Expr[GenOps[A]](
      q"""new _root_.dev.aliakovl.gin.GenOps[${c.weakTypeOf[A]}] {
            override val random = ${random
          .getOrElse(q"implicitly[${randomType(c.weakTypeOf[A])}]")}
            override val debug = $debug
          }"""
    )
  }

  def toRandom(tree: c.Tree): c.Tree = {
    q"new _root_.dev.aliakovl.gin.Random(() => $tree)"
  }

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

  case class GenTree(genClass: ClassSymbol, specs: List[(List[Optic], Tree)])

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
      case q"""$module[$from]($other).when[$to]""" => // проверить module
        Some(Prism(from.symbol, to.symbol), other)
      case _ => None
    }
  }

  sealed trait OpticsMerge
  case class ProductMerge(
      classSymbol: ClassSymbol,
      fields: Map[c.Symbol, OpticsMerge]
  ) extends OpticsMerge
  case class CoproductMerge(subclasses: Map[c.Symbol, OpticsMerge])
      extends OpticsMerge
  case class ApplyOptic(tree: c.Tree) extends OpticsMerge
  case class ONil(typeSymbol: c.Type) extends OpticsMerge

//  sealed trait RandomTree
//  case class Implicitly(tp: c.Type) extends RandomTree
//  case class Made(typ: c.Type) extends RandomTree

  def createDefs(
      types: Set[c.Type],
      resultTree: c.Tree,
      resultType: c.Type
  ): Unit = {
    val resultName = c.freshName("result")
    val blockTree: Map[String, c.Tree] = Map(resultName -> resultTree)
    val createdDefs: Map[c.Type, String] = Map(resultType -> resultName)
    c.info(
      c.enclosingPosition,
      mkStr(blockTree.mkString("\n"), createdDefs.mkString("\n")),
      force = true
    )
  }

  def mergeOptics(genTree: GenTree): Option[OpticsMerge] = {
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
                subclass -> ONil(
                  subclass.info.baseType(subclass.info.typeSymbol)
                )
              }
          }.toMap)
        case Lens(tn) :: tail =>
          ProductMerge(
            classSymbol,
            fields =
              publicConstructor(classSymbol).paramLists.flatten.map { param =>
                if (param.asTerm.name == tn) {
                  param -> help(
                    param.info.baseClasses.head.asClass,
                    tail,
                    tree
                  )
                } else {
                  param -> ONil(param.info.baseType(param.info.typeSymbol))
                }
              }.toMap
          )
        case Nil => ApplyOptic(tree)
      }
    }

    specs
      .map { case (optics, tree) => help(genClass, optics, tree) }
      .foldLeft(None: Option[OpticsMerge]) {
        case (None, om)       => Some(om)
        case (Some(lom), rom) => Some(mergeOptics(lom, rom))
      }
  }

  def mergeOptics(left: OpticsMerge, right: OpticsMerge): OpticsMerge = {
    (left, right) match {
      case (ProductMerge(_, l), ProductMerge(cr, r)) =>
        ProductMerge(
          cr,
          l.map { case (k, v) =>
            k -> mergeOptics(v, r(k))
          }
        )
      case (CoproductMerge(l), CoproductMerge(r)) =>
        CoproductMerge(l.map { case (k, v) =>
          k -> mergeOptics(v, r(k))
        })
      case (CoproductMerge(l), pm: ProductMerge) =>
        CoproductMerge(subclasses = l.map { case (subclass, ls) =>
          subclass -> mergeOptics(ls, pm)
        })
      case (lm: ProductMerge, CoproductMerge(r)) =>
        CoproductMerge(subclasses = r.map { case (subclass, rs) =>
          subclass -> mergeOptics(lm, rs)
        })
      case (m @ ProductMerge(_, fields), o: ONil) =>
        if (fields.nonEmpty) m else o
      case (m @ CoproductMerge(subclasses), o: ONil) =>
        if (subclasses.nonEmpty) m else o
      case (o: ONil, m @ ProductMerge(_, fields)) =>
        if (fields.nonEmpty) m else o
      case (o: ONil, m @ CoproductMerge(subclasses)) =>
        if (subclasses.nonEmpty) m else o
      case (_: ONil, or: ONil) => or
      case (_, _: ApplyOptic) | (_: ApplyOptic, _) =>
        c.abort(
          c.enclosingPosition,
          s"double application leads to erasure"
        )
    }
  }

  def findUnique(opticsMerge: OpticsMerge): Set[c.Type] = {
    opticsMerge match {
      case ProductMerge(_, fields) =>
        val (leaves, branches) = fields.partition(_._2.isInstanceOf[ONil])
        leaves.keySet.map { param =>
          param.info.baseType(param.info.typeSymbol)
        } union branches.values
          .flatMap(findUnique)
          .toSet
      case CoproductMerge(subclasses) =>
        val (leaves, branches) = subclasses.partition(_._2.isInstanceOf[ONil])
        leaves.keySet.map { subclass =>
          subclass.info.baseType(subclass.info.typeSymbol)
        } union branches.values.flatMap(findUnique).toSet
      case _ => Set.empty
    }
  }

  def mkTree(om: OpticsMerge): Tree = {
    om match {
      case ProductMerge(classSymbol, fields) =>
        q"${constructor(classSymbol)}( ..${fields.map { case field -> om =>
            q"$field = ${mkTree(om)}"
          }})"
      case CoproductMerge(subclasses) =>
        val size = subclasses.size
        q"scala.util.Random.nextInt($size) match { case ..${subclasses.zipWithIndex.map {
            case (symbol -> ApplyOptic(tree), index) =>
              cq"$index => $tree.get()"
            case (symbol -> ONil(typeSymbol), index) =>
              cq"$index => ${val impl = c.inferImplicitValue(randomType(typeSymbol))
                if (impl.nonEmpty) {
                  q"$impl.get()"
                } else {
                  q"implicitly[${randomType(typeSymbol)}].get()" // их нужно самим создать
                } }"
            case (symbol -> om, index) => cq"$index => ${mkTree(om)}"
          }} }"
      case ApplyOptic(tree) => q"$tree.get()"
      case ONil(typeSymbol) => {
        val impl = c.inferImplicitValue(randomType(typeSymbol))
        if (impl.nonEmpty) {
          q"$impl.get()"
        } else {
          q"implicitly[${randomType(typeSymbol)}].get()" // их нужно самим создать
        }
      }
    }
  }

  def randomType(symbolType: c.Type): c.Type =
    appliedType(typeOf[Random[_]].typeConstructor, symbolType)

  def constructor(classSymbol: ClassSymbol): c.Tree =
    Select(New(Ident(classSymbol)), termNames.CONSTRUCTOR)
}
