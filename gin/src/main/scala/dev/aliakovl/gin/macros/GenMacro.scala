package dev.aliakovl.gin.macros

import dev.aliakovl.gin.{Gen, Random}

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  type Vars = Map[c.Type, c.TermName]
  type Vals = Map[c.Type, Value]
  type VarsState[A] = State[Vars, A]
  type VState = (Vals, Vars)
  type FullState[A] = State[VState, A]

  val symbolGen = symbolOf[Gen.type].asClass.module
  val ginModule = c.mirror.staticModule("dev.aliakovl.gin.package")

  def initVars[A: c.WeakTypeTag]: Vars = Map(
    weakTypeOf[A] -> c.freshName(weakTypeOf[A].typeSymbol.name).toTermName
  )

  def materializeRandom[A: c.WeakTypeTag]: c.Expr[Random[A]] = {
    val (vars, vals) = initValues[A](None).run(initVars[A])

    c.Expr[Random[A]] {
      mkBlock[A](vars, vals)
    }
  }

  def randomImpl[A: c.WeakTypeTag]: c.Expr[Random[A]] = {
    val (vars, vals) = Option.when(weakTypeOf[A].typeSymbol.isClass) {
        disassembleTree(c.prefix.tree)
      }.fold(State.pure[Vars, Option[c.Tree]](None)) { ast =>
        mergeOptics[A](ast).map { om =>
          om.map(mkTree).map(toRandom)
        }
      }
      .flatMap(initValues[A])
      .run(initVars[A])

    c.Expr[Random[A]] {
      mkBlock[A](vars, vals)
    }
  }

  sealed trait OpticsMerge
  case class ProductMerge(classSymbol: ClassSymbol, fields: Map[c.TermName, OpticsMerge]) extends OpticsMerge
  case class CoproductMerge(subclasses: Map[c.Symbol, OpticsMerge]) extends OpticsMerge
  case class ApplyOptic(tree: Apply) extends OpticsMerge
  case class ONil(tree: c.TermName) extends OpticsMerge

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
          q"_root_.scala.util.Random.nextInt($size) match { case ..${subclasses.values.zipWithIndex
              .map { case name -> index =>
                cq"$index => ${callApply(q"$name")}"
              }} }"
        }
        q"lazy val ${variables(tp)}: _root_.dev.aliakovl.gin.Random[$tp] = $value"
    }

    q"{..$res; ${variables(weakTypeOf[A])}}"
  }

  def default[A: c.WeakTypeTag](tp: c.Type): FullState[Value] = {
    val rt = constructType[Random](tp)
    val implicitValue = c.inferImplicitValue(rt, withMacrosDisabled = true)
    if (implicitValue.nonEmpty && tp != weakTypeOf[A]) {
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
      c.inferImplicitValue(constructType[ValueOf](tp), withMacrosDisabled = true).nonEmpty
    ) {
      State.pure(CaseObject)
    } else if (
      tp.typeSymbol.isClass && !tp.typeSymbol.isAbstract && (tp.typeSymbol.asClass.isFinal || tp.typeSymbol.asClass.isCaseClass)
    ) {
      val params =
        paramListsOf(tp, publicConstructor(tp.typeSymbol.asClass, tp)).flatten
      State
        .traverse(params) { param =>
          for {
            vars <- State.get[VState].map(_._2)
            t = param.infoIn(tp)
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

  def help[A: WeakTypeTag](tp: c.Type): FullState[Value] = {
    State.get[VState].map(_._1).flatMap { values =>
      values.get(tp) match {
        case Some(value) => State.pure(value)
        case None =>
          for {
            value <- default[A](tp)
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
          _ <- State.modifyFirst[Vals, Vars](
            _.updated(weakTypeOf[A], Refer(value))
          )
          vars <- State.get[VState].map(_._2)
          _ <- State.traverse(vars.keySet - weakTypeOf[A])(help)
        } yield ()
      }
      .flatMap(_ => State.get[VState].map(_._1))
      .modifyState(_._2)((Map.empty, _))
  }

  type AST = List[Method]

  sealed trait Optic
  case class Lens(from: c.Type, to: c.TermName, tpe: c.Type) extends Optic
  case class Prism(from: c.Symbol, to: c.Symbol) extends Optic

  sealed trait Method
  case class Selector(optics: Optic, tail: Method) extends Method

  sealed trait Apply extends Method
  case class RandomApply(tree: c.Tree) extends Apply
  case class ConstApply(tree: c.Tree) extends Apply

  @tailrec
  final def disassembleTree[A: c.WeakTypeTag](tree: c.Tree, acc: AST = List.empty): AST = {
    tree match {
      case q"$other.specify[$_](($_) => $selector)($random)" =>
        val s = disassembleSelector(selector, RandomApply(random))
        disassembleTree(other, s +: acc)
      case q"$other.specifyConst[$_](($_) => $selector)($const)" =>
        val s = disassembleSelector(selector, ConstApply(const))
        disassembleTree(other, s +: acc)
      case q"$module.apply[$_]" if module.symbol == symbolGen => acc
      case _ => c.abort(c.enclosingPosition, "Shit happens...")
    }
  }

  @tailrec
  final def disassembleSelector(selector: c.Tree, acc: Method): Method = {
    selector match {
      case q"$other.$field" =>
        val t = selector.tpe.substituteTypes(List(selector.symbol), List(selector.tpe))
        val lens = Lens(other.tpe, field, t)
        disassembleSelector(other, Selector(lens, acc))
      case q"$module.GenWhen[$from]($other).when[$to]" if module.symbol == ginModule =>
        val prism = Prism(from.symbol, to.symbol)
        disassembleSelector(other, Selector(prism, acc))
      case _ => acc
    }
  }

  def helpMergeOptics(
      classSymbol: ClassSymbol,
      selector: Method
  ): VarsState[OpticsMerge] = {
    selector match {
      case Selector(Prism(_, to), tail) =>
        val subs = subclassesOf(to.asClass) + to
        State
          .traverse(subclassesOf(classSymbol)) { subclass =>
            if (subs.contains(subclass)) {
              helpMergeOptics(to.asClass, tail).map(om => subclass -> om)
            } else {
              val t = subclassType(subclass, classSymbol.toType)
              State
                .getOrElseUpdate(t, c.freshName(t.typeSymbol.name).toTermName)
                .map { name =>
                  subclass -> ONil(name)
                }
            }
          }
          .map(_.toMap)
          .map(CoproductMerge)
      case Selector(Lens(from, to, tpe), tail) =>
        val params =
          paramListsOf(from, publicConstructor(classSymbol, from)).flatten
        State
          .traverse(params) { param =>
            if (param.asTerm.name == to) {
              helpMergeOptics(tpe.typeSymbol.asClass, tail).map(om =>
                param.name.toTermName -> om
              )
            } else {
              val t = param.infoIn(from)
              State
                .getOrElseUpdate(t, c.freshName(t.typeSymbol.name).toTermName)
                .map { name =>
                  param.name.toTermName -> ONil(name)
                }
            }
          }
          .map(_.toMap)
          .map(ProductMerge(classSymbol, _))
      case arg: Apply => State.pure(ApplyOptic(arg))
    }
  }

  def mergeOptics[A: WeakTypeTag](ast: AST): VarsState[Option[OpticsMerge]] = {
    State.traverse(ast) { selector =>
        helpMergeOptics(weakTypeOf[A].typeSymbol.asClass, selector)
      }
      .map { list =>
        list.foldLeft(None: Option[OpticsMerge]) {
          case (None, om) => Some(om)
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
            case ApplyOptic(RandomApply(tree)) -> index =>
              cq"$index => ${callApply(tree)}"
            case ApplyOptic(ConstApply(tree)) -> index => cq"$index => $tree"
            case ONil(tree) -> index => cq"$index => ${callApply(q"$tree")}"
            case om -> index         => cq"$index => ${mkTree(om)}"
          }} }"
      case ApplyOptic(RandomApply(tree)) => callApply(tree)
      case ApplyOptic(ConstApply(tree))  => tree
      case ONil(tree)                   => callApply(q"$tree")
    }
  }

  def toRandom(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Random.apply($tree)"

  def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Random.const($tree)"

  def callApply(tree: c.Tree): c.Tree = q"$tree.apply()"

  def constructType[F[_]](tpe: c.Type)(implicit weakTypeTag: WeakTypeTag[F[_]]): c.Type = {
    appliedType(weakTypeTag.tpe.typeConstructor, tpe)
  }

  def constructor(classSymbol: ClassSymbol): c.Tree = {
    Select(New(Ident(classSymbol)), termNames.CONSTRUCTOR)
  }

  def subclassesOf(parent: ClassSymbol): Set[c.Symbol] = {
    val (abstractChildren, concreteChildren) =
      parent.knownDirectSubclasses
        .map { s =>
          s.info; s
        }
        .partition(_.isAbstract)

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

  def paramListsOf(tpe: c.Type, method: c.Symbol): List[List[c.universe.Symbol]] = {
    method.asMethod.infoIn(tpe).paramLists
  }

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
}
