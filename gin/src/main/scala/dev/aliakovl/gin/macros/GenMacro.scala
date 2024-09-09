package dev.aliakovl.gin.macros

import dev.aliakovl.gin.Random

import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  type Vars = Map[c.Type, c.TermName]
  type Vals = Map[c.Type, Value]
  type VarsState[A] = State[Vars, A]
  type VState = (Vals, Vars)
  type FullState[A] = State[VState, A]

  def materializeRandom[A: c.WeakTypeTag]: c.Expr[Random[A]] = {
    val initVars: Map[c.Type, TermName] = Map(
      weakTypeOf[A] -> c.freshName(weakTypeOf[A].typeSymbol.name).toTermName
    )

    c.Expr[Random[A]] {
      (mkBlock[A] _).tupled(initValues[A](None).run(initVars))
    }
  }

  def randomImpl[A: c.WeakTypeTag]: c.Expr[Random[A]] = {
    val initVars: Map[c.Type, TermName] = Map(
      weakTypeOf[A] -> c.freshName(weakTypeOf[A].typeSymbol.name).toTermName
    )

    c.Expr[Random[A]] {
      (mkBlock[A] _).tupled {
        disassembleTree(c.prefix.tree).fold(State.pure[Vars, Option[c.Tree]](None)) { genTree =>
          mergeOptics(genTree).map { om =>
            om.map(mkTree).map(toRandom)
          }
        }.flatMap(initValues[A]).run(initVars)
      }
    }
  }

  sealed trait Value
  case class Implicitly(rType: c.Type) extends Value
  case class Refer(value: c.Tree) extends Value
  case class CaseClass(fields: Map[c.Symbol, c.TermName]) extends Value
  case object CaseObject extends Value
  case class SealedTrait(subclasses: Map[c.Symbol, c.TermName]) extends Value

  def mkBlock[A: c.WeakTypeTag](variables: Vars, values: Vals): c.Tree = {
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

    q"{..$res; ${variables(weakTypeOf[A])}}"
  }

  def default(tp: c.Type): FullState[Value] = {
    val rt = randomType(tp)
    val implicitValue = c.inferImplicitValue(rt, withMacrosDisabled = true)
    if (implicitValue.nonEmpty) {
      State.pure(Refer(implicitValue))
    } else if (
      tp.typeSymbol.isAbstract && tp.typeSymbol.isClass && tp.typeSymbol.asClass.isSealed
    ) {
      val subclasses = subclassesOf(tp.typeSymbol.asClass)
      State
        .traverse(subclasses) { subclass =>
          for {
            vars <- State.get[VState].map(_._2)
            t = subclassType(subclass, tp)
            name <- vars
              .get(t)
              .fold {
                val value = c.freshName(t.typeSymbol.name).toTermName
                State.modifySecond[Vals, Vars](_.updated(t, value))
                  .zip(help(t))
                  .as(value)
              }(value => State.pure[VState, c.TermName](value))
          } yield subclass -> name
        }
        .map(_.toMap)
        .map(SealedTrait)
    } else if (
      c.inferImplicitValue(valueOfType(tp), withMacrosDisabled = true).nonEmpty
    ) {
      State.pure(CaseObject)
    } else if (
      tp.typeSymbol.isClass && (tp.typeSymbol.asClass.isFinal || tp.typeSymbol.asClass.isCaseClass)
    ) {
      val params =
        paramListsOf(tp, publicConstructor(tp.typeSymbol.asClass, tp)).flatten
      State
        .traverse(params) { param =>
          for {
            vars <- State.get[VState].map(_._2)
            t = param.typeSignatureIn(tp)
            name <- vars
              .get(t)
              .fold {
                val value = c.freshName(t.typeSymbol.name).toTermName
                State.modifySecond[Vals, Vars](_.updated(t, value))
                  .zip(help(t))
                  .as(value)
              }(value => State.pure[VState, c.TermName](value))
          } yield param -> name
        }
        .map(_.toMap)
        .map(CaseClass)
    } else {
      State.pure(Implicitly(tp))
    }

  }

  def help(tp: c.Type): FullState[Value] = {
    State.get[VState].map(_._1).flatMap { values =>
      values.get(tp) match {
        case Some(value) => State.pure(value)
        case None =>
          for {
            value <- default(tp)
            _ <- State.modifyFirst[Vals, Vars](_.updated(tp, value))
          } yield value
      }
    }
  }

  def initValues[A: c.WeakTypeTag](tree: Option[c.Tree]): VarsState[Vals] = {
    tree.fold {
      help(weakTypeOf[A]).flatMap { value =>
        State.modifyFirst[Vals, Vars](_.updated(weakTypeOf[A], value))
      }
    } { value =>
      for {
        _ <- State.modifyFirst[Vals, Vars](_.updated(weakTypeOf[A], Refer(value)))
        vars <- State.get[VState].map(_._2)
        _ <- State.traverse(vars.keySet - weakTypeOf[A])(help)
      } yield ()
    }
      .flatMap(_ => State.get[VState].map(_._1))
      .modifyState(_._2)((Map.empty, _))
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

  case class GenTree(genClass: ClassSymbol, specs: List[(List[Optic], Spec)])


  sealed trait Spec
  case class RandomSpec(tree: c.Tree) extends Spec
  case class ConstSpec(tree: c.Tree) extends Spec

  def disassembleTree[A: c.WeakTypeTag](tree: c.Tree): Option[GenTree] = {
    Option.when(weakTypeOf[A].typeSymbol.isClass) {
      val genClass = weakTypeOf[A].typeSymbol.asClass
      val specs: List[(List[Optic], Spec)] = List
        .unfold(tree) {
          case q"$other.specify[$_](($_) => $selector)($random)" =>
            Some((disassembleSelector(selector).reverse, RandomSpec(random)), other)
          case q"$other.specifyConst[$_](($_) => $selector)($const)" =>
            Some((disassembleSelector(selector).reverse, ConstSpec(const)), other)
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
  case class ApplyOptic(tree: Spec) extends OpticsMerge
  case class ONil(tree: c.TermName) extends OpticsMerge

  def helpMergeOptics(
      classSymbol: ClassSymbol,
      selector: List[Optic],
      tree: Spec
  ): VarsState[OpticsMerge] = {
    selector match {
      case Prism(_, to) :: tail =>
        val subs = subclassesOf(to.asClass) + to
        State
          .traverse(subclassesOf(classSymbol)) { subclass =>
            if (subs.contains(subclass)) {
              helpMergeOptics(to.asClass, tail, tree).map(om => subclass -> om)
            } else {
              val t = subclassType(subclass, classSymbol.toType)
              MState
                .getOrElseUpdate(t, c.freshName(t.typeSymbol.name).toTermName)
                .map { name =>
                  subclass -> ONil(name)
                }
            }
          }
          .map(_.toMap)
          .map(CoproductMerge)
      case Lens(from, to, tpe) :: tail =>
        val params = paramListsOf(from, publicConstructor(classSymbol, from)).flatten
        State
          .traverse(params) { param =>
            if (param.asTerm.name == to) {
              helpMergeOptics(tpe.typeSymbol.asClass, tail, tree).map(om =>
                param.name.toTermName -> om
              )
            } else {
              val t = param.typeSignatureIn(from)
              MState
                .getOrElseUpdate(t, c.freshName(t.typeSymbol.name).toTermName)
                .map { name =>
                  param.name.toTermName -> ONil(name)
                }
            }
          }
          .map(_.toMap)
          .map(ProductMerge(classSymbol, _))
      case Nil => State.pure(ApplyOptic(tree))
    }
  }

  def mergeOptics(genTree: GenTree): VarsState[Option[OpticsMerge]] = {
    val GenTree(genClass, specs) = genTree
    State.traverse(specs) { case (optics, tree) =>
      helpMergeOptics(genClass, optics, tree)
    }.map { list =>
      list.foldLeft(None: Option[OpticsMerge]) {
        case (None, om)       => Some(om)
        case (Some(lom), rom) => Some(mergeOptics(lom, rom))
      }
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
            case ApplyOptic(RandomSpec(tree)) -> index => cq"$index => ${callApply(tree)}"
            case ApplyOptic(ConstSpec(tree)) -> index => cq"$index => $tree"
            case ONil(tree) -> index => cq"$index => ${callApply(q"$tree")}"
            case om -> index         => cq"$index => ${mkTree(om)}"
          }} }"
      case ApplyOptic(RandomSpec(tree)) => callApply(tree)
      case ApplyOptic(ConstSpec(tree)) => tree
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
