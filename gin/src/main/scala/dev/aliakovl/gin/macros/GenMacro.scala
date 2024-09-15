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

  val genSymbol = symbolOf[Gen.type].asClass.module
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
      }.fold(State.pure[Vars, Option[c.Tree]](None)) { methods =>
        mergeOptics[A](methods).map(_.map(mkTree).map(toRandom))
      }.flatMap(initValues[A])
      .run(initVars[A])

    c.Expr[Random[A]] {
      mkBlock[A](vars, vals)
    }
  }

  sealed trait Value
  case class Implicitly(tpe: c.Type) extends Value
  case class Refer(value: c.Tree) extends Value
  case class CaseClass(fields: List[List[c.TermName]]) extends Value
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
        val args = fields.map(_.map(name => callApply(q"$name")))
        construct(tp, args)
        val value = toRandom(construct(tp, args))
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

  def default[A: c.WeakTypeTag](tpe: c.Type): FullState[Value] = {
    val rt = constructType[Random](tpe)
    val implicitValue = c.inferImplicitValue(rt, withMacrosDisabled = true)
    if (implicitValue.nonEmpty && tpe != weakTypeOf[A]) {
      State.pure(Refer(implicitValue))
    } else if (tpe.typeSymbol.isAbstract && tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
      val subclasses = subclassesOf(tpe.typeSymbol.asClass)
      State.traverse(subclasses) { subclass =>
        for {
          vars <- State.get[VState].map(_._2)
          t = subclassType(subclass, tpe)
          name <- vars
            .get(t)
            .fold {
              val value = c.freshName(t.typeSymbol.name).toTermName
              State.modifySecond[Vals, Vars](_.updated(t, value)).zip(help(t)).as(value)
            }(value => State.pure[VState, c.TermName](value))
        } yield subclass -> name
      }.map(_.toMap).map(SealedTrait)
    } else if (c.inferImplicitValue(constructType[ValueOf](tpe), withMacrosDisabled = true).nonEmpty) {
      State.pure(CaseObject)
    } else if (tpe.typeSymbol.isClass && !tpe.typeSymbol.isAbstract && (tpe.typeSymbol.asClass.isFinal || tpe.typeSymbol.asClass.isCaseClass)) {
      State.sequence {
        paramListsOf(publicConstructor(tpe.typeSymbol.asClass), tpe).map { params =>
          State.traverse(params) { param =>
            for {
              vars <- State.get[VState].map(_._2)
              t = param.infoIn(tpe)
              name <- vars
                .get(t)
                .fold {
                  val value = c.freshName(t.typeSymbol.name).toTermName
                  State.modifySecond[Vals, Vars](_.updated(t, value)).zip(help(t)).as(value)
                }(value => State.pure[VState, c.TermName](value))
            } yield name
          }
        }
      }.map(CaseClass)
    } else {
      State.pure(Implicitly(tpe))
    }
  }

  def help[A: WeakTypeTag](tpe: c.Type): FullState[Value] = {
    State.get[VState].map(_._1).flatMap { values =>
      values.get(tpe) match {
        case Some(value) => State.pure(value)
        case None =>
          for {
            value <- default[A](tpe)
            _ <- State.modifyFirst[Vals, Vars](_.updated(tpe, value))
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

  type Methods = List[Method]

  sealed trait Optic
  case class Lens(from: c.Type, to: c.TermName, tpe: c.Type) extends Optic
  case class Prism(from: c.Symbol, to: c.Symbol) extends Optic

  sealed trait Method
  case class Selector(optics: Optic, tail: Method) extends Method

  sealed trait Apply extends Method
  case class RandomApply(tree: c.Tree) extends Apply
  case class ConstApply(tree: c.Tree) extends Apply

  @tailrec
  final def disassembleTree[A: c.WeakTypeTag](tree: c.Tree, acc: Methods = List.empty): Methods = {
    tree match {
      case q"$other.specify[$_](($_) => $selector)($random)" =>
        val s = disassembleSelector(selector, RandomApply(random))
        disassembleTree(other, s +: acc)
      case q"$other.specifyConst[$_](($_) => $selector)($const)" =>
        val s = disassembleSelector(selector, ConstApply(const))
        disassembleTree(other, s +: acc)
      case q"$module.apply[$_]" if module.symbol == genSymbol => acc
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

  sealed trait SpecifiedRandom
  case class SpecifiedCaseClass(classSymbol: ClassSymbol, fields: List[Map[c.TermName, SpecifiedRandom]]) extends SpecifiedRandom
  case class SpecifiedSealedTrait(subclasses: Map[c.Symbol, SpecifiedRandom]) extends SpecifiedRandom
  case class Specified(tree: Apply) extends SpecifiedRandom
  case class NotSpecified(tree: c.TermName) extends SpecifiedRandom

  def helpMergeOptics(
      classSymbol: ClassSymbol,
      selector: Method
  ): VarsState[SpecifiedRandom] = {
    selector match {
      case Selector(Prism(_, to), tail) =>
        val subs = subclassesOf(to.asClass) + to
        State.traverse(subclassesOf(classSymbol)) { subclass =>
          if (subs.contains(subclass)) {
            helpMergeOptics(to.asClass, tail).map(om => subclass -> om)
          } else {
            val t = subclassType(subclass, classSymbol.toType)
            State
              .getOrElseUpdate(t, c.freshName(t.typeSymbol.name).toTermName)
              .map { name =>
                subclass -> NotSpecified(name)
              }
          }
        }.map(_.toMap).map(SpecifiedSealedTrait)
      case Selector(Lens(from, to, tpe), tail) =>
        State.sequence {
          paramListsOf(publicConstructor(classSymbol), from).map { params =>
            State.traverse(params) { param =>
              if (param.asTerm.name == to) {
                helpMergeOptics(tpe.typeSymbol.asClass, tail).map(om =>
                  param.name.toTermName -> om
                )
              } else {
                val t = param.infoIn(from)
                State.getOrElseUpdate(t, c.freshName(t.typeSymbol.name).toTermName).map { name =>
                  param.name.toTermName -> NotSpecified(name)
                }
              }
            }.map(_.toMap)
          }
        }.map(SpecifiedCaseClass(classSymbol, _))

      case arg: Apply => State.pure(Specified(arg))
    }
  }

  def mergeOptics[A: WeakTypeTag](ast: Methods): VarsState[Option[SpecifiedRandom]] = {
    State.traverse(ast) { selector =>
        helpMergeOptics(weakTypeOf[A].typeSymbol.asClass, selector)
    }.map { list =>
      list.foldLeft[Option[SpecifiedRandom]](None) {
        case (Some(left), right) => Some(mergeSpecifications(left, right))
        case (None, value) => Some(value)
      }
    }
  }

  def mergeSpecifications(
    left: SpecifiedRandom,
    right: SpecifiedRandom
  ): SpecifiedRandom = (left, right) match {
    case (SpecifiedCaseClass(leftClass, leftFields), SpecifiedCaseClass(rightClass, rightFields)) =>
      assert(leftClass == rightClass)
      val fields = leftFields.zip(rightFields).map { case (leftArgs, rightArgs) =>
        leftArgs.map { case (key, value) =>
          key -> mergeSpecifications(value, rightArgs(key))
        }
      }
      SpecifiedCaseClass(rightClass, fields)
    case (SpecifiedSealedTrait(leftSubclasses), SpecifiedSealedTrait(rightSubclasses)) =>
      SpecifiedSealedTrait(leftSubclasses.map { case (key, value) =>
        key -> mergeSpecifications(value, rightSubclasses(key))
      })
    case (SpecifiedSealedTrait(subclasses), _: SpecifiedCaseClass) =>
      SpecifiedSealedTrait(subclasses.map { case (subclass, leftSpecified) =>
        subclass -> mergeSpecifications(leftSpecified, right)
      })
    case (_: SpecifiedCaseClass, SpecifiedSealedTrait(subclasses)) =>
      SpecifiedSealedTrait(subclasses.map { case (subclass, rightSpecified) =>
        subclass -> mergeSpecifications(left, rightSpecified)
      })
    case (SpecifiedCaseClass(_, fields), _: NotSpecified) =>
      if (fields.nonEmpty) left else right
    case (SpecifiedSealedTrait(subclasses), _: NotSpecified) =>
      if (subclasses.nonEmpty) left else right
    case (_: NotSpecified, SpecifiedCaseClass(_, fields)) =>
      if (fields.nonEmpty) right else left
    case (_: NotSpecified, SpecifiedSealedTrait(subclasses)) =>
      if (subclasses.nonEmpty) right else left
    case (_: NotSpecified, _: NotSpecified) => right
    case (_: NotSpecified, _: Specified) => right
    case (_: Specified, _: NotSpecified) => left
    case _ => c.abort(c.enclosingPosition, s"Some specifications conflict")
  }

  def mkTree(sr: SpecifiedRandom): c.Tree = sr match {
    case SpecifiedCaseClass(classSymbol, fields) =>
      val args = fields.map(_.values.map(mkTree).toList)
      construct(classSymbol.toType, args)
    case SpecifiedSealedTrait(subclasses) =>
      val size = subclasses.size
      q"_root_.scala.util.Random.nextInt($size) match { case ..${subclasses.values.zipWithIndex.map {
          case Specified(RandomApply(tree)) -> index =>
            cq"$index => ${callApply(tree)}"
          case Specified(ConstApply(tree)) -> index => cq"$index => $tree"
          case NotSpecified(tree) -> index => cq"$index => ${callApply(q"$tree")}"
          case om -> index         => cq"$index => ${mkTree(om)}"
        }} }"
    case Specified(RandomApply(tree)) => callApply(tree)
    case Specified(ConstApply(tree))  => tree
    case NotSpecified(tree)           => callApply(q"$tree")
  }

  def toRandom(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Random.apply($tree)"

  def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Random.const($tree)"

  def callApply(tree: c.Tree): c.Tree = q"$tree.apply()"

  def constructType[F[_]](tpe: c.Type)(implicit weakTypeTag: WeakTypeTag[F[_]]): c.Type = {
    appliedType(weakTypeTag.tpe.typeConstructor, tpe)
  }

  def construct(tpe: c.Type, constructorArgs: List[List[c.Tree]]): c.Tree = {
    val constructionMethodTree: Tree = Select(New(Ident(tpe.dealias.typeSymbol)), termNames.CONSTRUCTOR)
    constructorArgs.foldLeft(constructionMethodTree)(Apply.apply)
  }

  def subclassesOf(parent: ClassSymbol): Set[c.Symbol] = {
    val (abstractChildren, concreteChildren) =
      parent.knownDirectSubclasses
        .map { s =>
          s.info; s
        }
        .partition(_.isAbstract)

    concreteChildren.foreach { child =>
      if (!child.info.typeSymbol.asClass.isFinal && !child.info.typeSymbol.asClass.isCaseClass) {
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

  def paramListsOf(method: c.Symbol, tpe: c.Type): List[List[c.universe.Symbol]] = {
    method.asMethod.infoIn(tpe).paramLists
  }

  def publicConstructor(parent: ClassSymbol): MethodSymbol = {
    val members = parent.info.members
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
