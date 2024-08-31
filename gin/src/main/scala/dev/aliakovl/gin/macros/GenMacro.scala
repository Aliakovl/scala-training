package dev.aliakovl.gin.macros

import dev.aliakovl.gin.{Gen, GenOps, Random}

import scala.collection.mutable
import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  private val variables: mutable.Map[c.Type, c.TermName] = mutable.Map()

  def randomImpl[A: c.WeakTypeTag](gen: c.Expr[Gen[A]]): c.Expr[GenOps[A]] = {
    val a: Option[c.Tree] = disassembleTree(gen)
      .flatMap(mergeOptics[A])
      .map(mkTree)
      .map(toRandom)
      .map(mkBlock[A])

    a.foreach(t => c.info(c.enclosingPosition, show(t), force = true))

    c.info(
      c.enclosingPosition,
      variables.mkString("\n", "\n", "\n"),
      force = true
    )

    mkGenOps[A](a, a.map(show(_)).getOrElse(""))
  }

  def mkBlock[A: c.WeakTypeTag](tree: c.Tree): c.Tree = {
    val res = variables.map {
      case (tp, tn) if tp == weakTypeOf[A] =>
        q"lazy val $tn: _root_.dev.aliakovl.gin.Random[$tp] = $tree"
      case (tp, tn) =>
        val rt = randomType(tp)
        val implicitValue = c.inferImplicitValue(rt)
        if (implicitValue.nonEmpty) {
          q"lazy val $tn: _root_.dev.aliakovl.gin.Random[$tp] = $implicitValue"
        } else {
          q"lazy val $tn: _root_.dev.aliakovl.gin.Random[$tp] = implicitly[$rt]"
        }
    }

    q"{..$res; ${variables(weakTypeOf[A])}}"
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

  def disassembleTree[A: WeakTypeTag](tree: c.Expr[Gen[A]]): Option[GenTree] = {
    Option.when(symbolOf[A].isClass) {
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
  case class ONil(tree: c.TermName) extends OpticsMerge

  def mergeOptics[A: c.WeakTypeTag](genTree: GenTree): Option[OpticsMerge] = {
    val GenTree(genClass, specs) = genTree

    variables.addOne(
      weakTypeOf[A] -> c.freshName(weakTypeOf[A].typeSymbol.name).toTermName
    )

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
                val t = subclass.info.baseType(subclass.info.typeSymbol)
                val name = variables.getOrElseUpdate(
                  t,
                  c.freshName(t.typeSymbol.name).toTermName
                )
                subclass -> ONil(name)
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
                  val t = param.info.baseType(param.info.typeSymbol)
                  val name = variables.getOrElseUpdate(
                    t,
                    c.freshName(t.typeSymbol.name).toTermName
                  )
                  param -> ONil(name)
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
        q"_root_.scala.util.Random.nextInt($size) match { case ..${subclasses.zipWithIndex.map {
            case (symbol -> ApplyOptic(tree), index) =>
              cq"$index => $tree.get()"
            case (symbol -> ONil(tree), index) => cq"$index => $tree.get()"
            case (symbol -> om, index)         => cq"$index => ${mkTree(om)}"
          }} }"
      case ApplyOptic(tree) => q"$tree.get()"
      case ONil(tree)       => q"$tree.get()"
    }
  }

  def randomType(symbolType: c.Type): c.Type =
    appliedType(typeOf[Random[_]].typeConstructor, symbolType)

  def constructor(classSymbol: ClassSymbol): c.Tree =
    Select(New(Ident(classSymbol)), termNames.CONSTRUCTOR)
}
