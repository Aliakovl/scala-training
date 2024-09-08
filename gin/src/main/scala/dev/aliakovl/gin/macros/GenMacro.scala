package dev.aliakovl.gin.macros

import dev.aliakovl.gin.Random

import scala.collection.mutable
import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  private val variables: mutable.Map[c.Type, c.TermName] = mutable.Map()

  private var resultType: c.Type = null

  def oneOfImpl[A: c.WeakTypeTag](values: c.Expr[Random[Any]]*): c.Expr[Random[A]] = {
    c.abort(c.enclosingPosition, show(values))
  }

  def materializeRandom[A: c.WeakTypeTag]: c.Expr[Random[A]] = {
    resultType = weakTypeOf[A]

    variables.getOrElseUpdate(
      resultType,
      c.freshName(resultType.typeSymbol.name).toTermName
    )

    c.Expr[Random[A]] {
      mkBlock[A](
        initValues[A](None)
      )
    }
  }

  def randomImpl[A: c.WeakTypeTag]: c.Expr[Random[A]] = {
    resultType = weakTypeOf[A]

    variables.getOrElseUpdate(
      resultType,
      c.freshName(resultType.typeSymbol.name).toTermName
    )

    c.Expr[Random[A]] {
      mkBlock[A](
        initValues[A](
          disassembleTree(c.prefix.tree)
            .flatMap(mergeOptics[A])
            .map(mkTree)
            .map(toRandom)
        )
      )
    }
  }

  sealed trait Value
  case class Implicitly(rType: c.Type) extends Value
  case class Refer(value: c.Tree) extends Value
  case class CaseClass(fields: Map[c.Symbol, c.TermName]) extends Value
  case object CaseObject extends Value
  case class SealedTrait(subclasses: Map[c.Symbol, c.TermName]) extends Value

  def mkBlock[A: c.WeakTypeTag](values: Map[c.Type, Value]): c.Tree = {
    val res = values.map {
      case tp -> Implicitly(rType) =>
        q"lazy val ${variables(tp)}: _root_.dev.aliakovl.gin.Random[$tp] = _root_.scala.Predef.implicitly[$rType]"
      case tp -> Refer(value) =>
        q"lazy val ${variables(tp)}: _root_.dev.aliakovl.gin.Random[$tp] = ${c.untypecheck(value.duplicate)}"
      case tp -> CaseObject =>
        q"lazy val ${variables(tp)}: _root_.dev.aliakovl.gin.Random[$tp] = ${toConst(q"_root_.scala.Predef.valueOf[$tp]")}"
      case tp -> CaseClass(fields) =>
        val value = toRandom {
          q"${constructor(tp.typeSymbol.asClass)}( ..${fields.map { case field -> name =>
              q"$field = ${callApply(q"$name")}"
            }})"
        }
        q"lazy val ${variables(tp)}: _root_.dev.aliakovl.gin.Random[$tp] = $value"
      case tp -> SealedTrait(subclasses) =>
        val size = subclasses.size
        val value = toRandom {
          q"_root_.scala.util.Random.nextInt($size) match { case ..${subclasses.zipWithIndex
              .map { case (symbol -> name, index) =>
                cq"$index => ${callApply(q"$name")}"
              }} }"
        }
        q"lazy val ${variables(tp)}: _root_.dev.aliakovl.gin.Random[$tp] = $value"
    }

    q"{..$res; ${variables(resultType)}}"
  }

  def isConstantType(tpe: c.Type): Boolean = {
    tpe match {
      case ConstantType(_) => true
      case SingleType(_, _) => true
      case _ => false
    }
  }

  def initValues[A: c.WeakTypeTag](tree: Option[c.Tree]): Map[c.Type, Value] = {
    val values: mutable.Map[c.Type, Value] = mutable.Map.empty

    def default(tp: c.Type): Value = {
      val rt = randomType(tp)
      val implicitValue = c.inferImplicitValue(rt, withMacrosDisabled = true)
      if (implicitValue.nonEmpty) {
        Refer(implicitValue)
      } else if (
        tp.typeSymbol.isAbstract && tp.typeSymbol.isClass && tp.typeSymbol.asClass.isSealed
      ) {
        val subclasses = subclassesOf(tp.typeSymbol.asClass)
        SealedTrait(subclasses = subclasses.map { subclass =>
          val t = subclassType(subclass, tp)
          val name = variables.get(t) match {
            case Some(value) => value
            case None =>
              val value = c.freshName(t.typeSymbol.name).toTermName
              variables.addOne(t -> value)
              help(t)
              value
          }
          subclass -> name
        }.toMap)
      } else if (c.inferImplicitValue(valueOfType(tp), withMacrosDisabled = true).nonEmpty) {
        CaseObject
      } else if (
        tp.typeSymbol.isClass && (tp.typeSymbol.asClass.isFinal || tp.typeSymbol.asClass.isCaseClass)
      ) {
        val params =
          paramListsOf(tp, publicConstructor(tp.typeSymbol.asClass, tp)).flatten
        CaseClass(fields = params.map { param =>
          val t = param.typeSignatureIn(tp)
          val name = variables.get(t) match {
            case Some(value) => value
            case None =>
              val value = c.freshName(t.typeSymbol.name).toTermName
              variables.addOne(t -> value)
              help(t)
              value
          }
          param -> name
        }.toMap)
      } else {
        Implicitly(tp)
      }
    }

    def help(tp: c.Type): Value = {
      values.getOrElseUpdate(tp, default(tp))
    }

    tree match {
      case Some(value) =>
        values.addOne(resultType -> Refer(value))
        variables.keys.filter(_ != resultType).foreach { t =>
          help(t)
        }
      case None => values.addOne(resultType -> help(resultType))
    }
    values.toMap
  }

  def toRandom(tree: c.Tree): c.Tree = {
    q"_root_.dev.aliakovl.gin.Random($tree)"
  }

  def toConst(tree: c.Tree): c.Tree = {
    q"_root_.dev.aliakovl.gin.Random.const($tree)"
  }

  def subclassesOf(parent: ClassSymbol): Set[c.Symbol] = {
    val (abstractChildren, concreteChildren) =
      parent.knownDirectSubclasses.partition(_.isAbstract)

    concreteChildren.foreach { child =>
      if (
        !child.info.typeSymbol.asClass.isFinal && !child.info.typeSymbol.asClass.isCaseClass
      ) {
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

  def subclassType(subclass: c.Symbol, parent: c.Type): c.Type = {
    val sEta = subclass.asType.toType.etaExpand
    sEta.finalResultType.substituteTypes(
      from = sEta
        .baseType(parent.typeSymbol)
        .typeArgs
        .map(_.typeSymbol),
      to = parent.typeArgs
    )
  }

  def paramListsOf(
      tpe: c.Type,
      method: c.Symbol
  ): List[List[c.universe.Symbol]] =
    method.asMethod.typeSignatureIn(tpe).paramLists

  def publicConstructor(parent: ClassSymbol, tpe: c.Type): MethodSymbol = {
    val members = parent.infoIn(tpe).members
    members
      .find(m => m.isMethod && m.asMethod.isPrimaryConstructor && m.isPublic)
      .orElse(
        members.find(m => m.isMethod && m.asMethod.isConstructor && m.isPublic)
      )
      .map(_.asMethod)
      .getOrElse {
        c.abort(
          c.enclosingPosition,
          s"class ${parent.name.decodedName} has no public constructors"
        )
      }
  }

  sealed trait Optic
  case class Lens(from: c.Type, to: c.TermName, tpe: c.Type) extends Optic
  case class Prism(from: c.Symbol, to: c.Symbol) extends Optic

  case class GenTree(genClass: ClassSymbol, specs: List[(List[Optic], Tree)])

  def disassembleTree(tree: c.Tree): Option[GenTree] = {
    Option.when(resultType.typeSymbol.isClass) {
      val genClass = resultType.typeSymbol.asClass
      val specs: List[(List[Optic], c.Tree)] = List
        .unfold(tree) {
          case q"$other.specify[..$_](($_) => $selector)($random)" =>
            Some((disassembleSelector(selector).reverse, random), other)
          case q"${Ident(TermName("Gen"))}.apply[$_]" => None
        }
        .reverse
      GenTree(genClass, specs)
    }
  }

  def disassembleSelector(selector: c.Tree): List[Optic] = {
    List.unfold(selector) {
      case a @ q"$other.${field: TermName}" =>
        val t = a.tpe.substituteTypes(List(a.symbol), List(selector.tpe))
        Some(Lens(other.tpe, field, t), other)
      case q"""${Select(Select(This(TypeName("gin")), termNames.PACKAGE), TermName("GenWhen"))}[$from]($other).when[$to]""" => Some(Prism(from.symbol, to.symbol), other)
      case _ => None
    }
  }

  sealed trait OpticsMerge
  case class ProductMerge(
      classSymbol: ClassSymbol,
      fields: Map[c.TermName, OpticsMerge]
  ) extends OpticsMerge
  case class CoproductMerge(subclasses: Map[c.Symbol, OpticsMerge])
      extends OpticsMerge
  case class ApplyOptic(tree: c.Tree) extends OpticsMerge
  case class ONil(tree: c.TermName) extends OpticsMerge

  def mergeOptics[A: c.WeakTypeTag](genTree: GenTree): Option[OpticsMerge] = {
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
                val t = subclassType(subclass, classSymbol.toType)
                val name = variables.getOrElseUpdate(
                  t,
                  c.freshName(t.typeSymbol.name).toTermName
                )
                subclass -> ONil(name)
              }
          }.toMap)
        case Lens(from, to, tpe) :: tail =>
          ProductMerge(
            classSymbol,
            fields = paramListsOf(
              from,
              publicConstructor(classSymbol, from)
            ).flatten.map { param =>
              if (param.asTerm.name == to) {
                param.name.toTermName -> help(
                  tpe.typeSymbol.asClass,
                  tail,
                  tree
                )
              } else {
                val t = param.typeSignatureIn(from)
                val name = variables.getOrElseUpdate(
                  t,
                  c.freshName(t.typeSymbol.name).toTermName
                )
                param.name.toTermName -> ONil(name)
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
      case (_: ONil, or: ONil)      => or
      case (_: ONil, a: ApplyOptic) => a
      case (a: ApplyOptic, _: ONil) => a
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"double application leads to erasure"
        )
    }
  }

  def mkTree(om: OpticsMerge): c.Tree = {
    om match {
      case ProductMerge(classSymbol, fields) =>
        q"${constructor(classSymbol)}( ..${fields.map { case field -> om =>
            q"$field = ${mkTree(om)}"
          }})"
      case CoproductMerge(subclasses) =>
        val size = subclasses.size
        q"_root_.scala.util.Random.nextInt($size) match { case ..${subclasses.values.zipWithIndex.map {
            case ApplyOptic(tree) -> index => cq"$index => ${callApply(tree)}"
            case ONil(tree) -> index => cq"$index => ${callApply(q"$tree")}"
            case om -> index         => cq"$index => ${mkTree(om)}"
          }} }"
      case ApplyOptic(tree) => callApply(tree)
      case ONil(tree)       => callApply(q"$tree")
    }
  }

  def callApply(tree: c.Tree): c.Tree = q"$tree.apply()"

  def randomType(symbolType: c.Type): c.Type =
    appliedType(typeOf[Random[_]].typeConstructor, symbolType)

  def valueOfType(symbolType: c.Type): c.Type =
    appliedType(typeOf[ValueOf[_]].typeConstructor, symbolType)

  def constructor(classSymbol: ClassSymbol): c.Tree =
    Select(New(Ident(classSymbol)), termNames.CONSTRUCTOR)
}
