package dev.aliakovl.gin.macros

import dev.aliakovl.gin
import dev.aliakovl.gin.Gen

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

class GenMacro(val c: blackbox.Context) {
  import c.universe._

  type Variables = Map[c.Type, c.TermName]
  type Values = Map[c.Type, Value]
  type VarsState[A] = State[Variables, A]
  type VState = (Variables, Values)
  type FullState[A] = State[VState, A]

  val genSymbol = symbolOf[gin.Gen.type].asClass.module
  val ginModule = c.mirror.staticModule("dev.aliakovl.gin.package")

  def initVars[A: c.WeakTypeTag]: Variables = Map(
    weakTypeOf[A] -> c.freshName(weakTypeOf[A].typeSymbol.name).toTermName
  )

  def materializeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = {
    val (vars, vals) = initValues[A](None).run(initVars[A])

    c.Expr[Gen[A]] {
      mkBlock[A](vars, vals)
    }
  }

  def makeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = {
    val (vars, vals) = Option.when(weakTypeOf[A].typeSymbol.isClass) {
        disassembleTree(c.prefix.tree)
      }.fold(State.pure[Variables, Option[c.Tree]](None)) { methods =>
        mergeMethods[A](methods).map(_.map(specifiedTree).map(toGen))
      }.flatMap(initValues[A])
      .run(initVars[A])

    c.Expr[Gen[A]] {
      mkBlock[A](vars, vals)
    }
  }

  sealed trait Value
  case class Implicitly(tpe: c.Type) extends Value
  case class Refer(value: c.Tree) extends Value
  case class CaseClass(fields: List[List[c.TermName]]) extends Value
  case object CaseObject extends Value
  case class SealedTrait(subclasses: Map[c.Symbol, c.TermName]) extends Value

  def mkBlock[A: c.WeakTypeTag](variables: Variables, values: Values): c.Tree = {
    def lazyVal(tpe: c.Type, value: c.Tree): c.Tree = q"lazy val ${variables(tpe)}: _root_.dev.aliakovl.gin.Gen[$tpe] = $value"

    val declaration = values.map {
      case tp -> Implicitly(rType) => lazyVal(tp, implicitly(rType))
      case tp -> Refer(value) => lazyVal(tp, c.untypecheck(value.duplicate))
      case tp -> CaseObject => lazyVal(tp, constValueOf(tp))
      case tp -> CaseClass(fields) =>
        val args = fields.map(_.map(name => callApply(Ident(name))))
        construct(tp, args)
        val value = toGen(construct(tp, args))
        lazyVal(tp, value)
      case tp -> SealedTrait(subclasses) =>
        val value = toGen(constructCases(subclasses.values){ name => callApply(Ident(name)) })
        lazyVal(tp, value)
    }

    q"{..$declaration; ${variables(weakTypeOf[A])}}"
  }

  def default[A: c.WeakTypeTag](tpe: c.Type): FullState[Value] = {
    val rt = constructType[Gen](tpe)
    val implicitValue = c.inferImplicitValue(rt, withMacrosDisabled = true)
    if (implicitValue.nonEmpty && tpe != weakTypeOf[A]) {
      State.pure(Refer(implicitValue))
    } else if (tpe.typeSymbol.isAbstract && tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
      val subtypes = subclassesOf(tpe.typeSymbol.asClass).map(subclassType(_, tpe))
      State.traverse(subtypes) { subtype =>
        for {
          vars <- State.get[VState].map(_._1)
          name <- vars.get(subtype).fold {
              val value = c.freshName(subtype.typeSymbol.name).toTermName
              State.modifyFirst[Variables, Values](_.updated(subtype, value)).zip(help(subtype)).as(value)
            }(value => State.pure[VState, c.TermName](value))
        } yield subtype.typeSymbol -> name
      }.map(_.toMap).map(SealedTrait)
    } else if (c.inferImplicitValue(constructType[ValueOf](tpe), withMacrosDisabled = true).nonEmpty) {
      State.pure(CaseObject)
    } else if (tpe.typeSymbol.isClass && !tpe.typeSymbol.isAbstract && (tpe.typeSymbol.asClass.isFinal || tpe.typeSymbol.asClass.isCaseClass)) {
      State.sequence {
        paramListsOf(publicConstructor(tpe), tpe).map { params =>
          State.traverse(params) { param =>
            for {
              vars <- State.get[VState].map(_._1)
              paramType = param.info
              name <- vars
                .get(paramType)
                .fold {
                  val value = c.freshName(paramType.typeSymbol.name).toTermName
                  State.modifyFirst[Variables, Values](_.updated(paramType, value)).zip(help(paramType)).as(value)
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
    State.get[VState].map(_._2).flatMap { values =>
      values.get(tpe) match {
        case Some(value) => State.pure(value)
        case None =>
          for {
            value <- default[A](tpe)
            _ <- State.modifySecond[Variables, Values](_.updated(tpe, value))
          } yield value
      }
    }
  }

  def initValues[A: c.WeakTypeTag](tree: Option[c.Tree]): VarsState[Values] = {
    tree.fold {
        help(weakTypeOf[A]).flatMap { value =>
          State.modifySecond[Variables, Values](_.updated(weakTypeOf[A], value))
        }
      } { value =>
        for {
          _ <- State.modifySecond[Variables, Values](
            _.updated(weakTypeOf[A], Refer(value))
          )
          vars <- State.get[VState].map(_._1)
          _ <- State.traverse(vars.keySet - weakTypeOf[A])(help)
        } yield ()
      }
      .flatMap(_ => State.get[VState].map(_._2))
      .modifyState(_._1)((_, Map.empty))
  }

  type Methods = List[Method]

  sealed trait Optic
  case class Lens(from: c.Type, to: c.TermName, tpe: c.Type) extends Optic
  case class Prism(from: c.Symbol, to: c.Symbol) extends Optic

  sealed trait Method
  case class Selector(optics: Optic, tail: Method) extends Method

  sealed trait Arg extends Method
  case class GenArg(tree: c.Tree) extends Arg
  case class ConstArg(tree: c.Tree) extends Arg

  @tailrec
  final def disassembleTree[A: c.WeakTypeTag](tree: c.Tree, acc: Methods = List.empty): Methods = {
    tree match {
      case q"$other.specify[$_](($_) => $selector)($gen)" =>
        val s = disassembleSelector(selector, GenArg(gen))
        disassembleTree(other, s +: acc)
      case q"$other.specifyConst[$_](($_) => $selector)($const)" =>
        val s = disassembleSelector(selector, ConstArg(const))
        disassembleTree(other, s +: acc)
      case q"$module.custom[$_]" if module.symbol == genSymbol => acc
      case _ => c.abort(c.enclosingPosition, "Unsupported syntax.")
    }
  }

  @tailrec
  final def disassembleSelector(selector: c.Tree, acc: Method): Method = {
    selector match {
      case q"$other.$field" =>
        val lens = Lens(other.tpe, field, selector.tpe)
        disassembleSelector(other, Selector(lens, acc))
      case q"$module.GenWhen[$from]($other).when[$to]" if module.symbol == ginModule =>
        val prism = Prism(from.symbol, to.symbol)
        disassembleSelector(other, Selector(prism, acc))
      case _: Ident => acc
      case _ => c.abort(c.enclosingPosition, "Unsupported path element.")
    }
  }

  sealed trait SpecifiedGen
  case class SpecifiedCaseClass(classSymbol: ClassSymbol, fields: List[Map[c.TermName, SpecifiedGen]]) extends SpecifiedGen
  case class SpecifiedSealedTrait(subclasses: Map[c.Symbol, SpecifiedGen]) extends SpecifiedGen
  case class Specified(tree: Arg) extends SpecifiedGen
  case class NotSpecified(tree: c.TermName) extends SpecifiedGen

  def helpMergeOptics(
      classSymbol: ClassSymbol,
      selector: Method
  ): VarsState[SpecifiedGen] = {
    selector match {
      case Selector(Prism(_, to), tail) =>
        if (!classSymbol.isSealed) c.abort(c.enclosingPosition, s"$classSymbol is not sealed")
        val subtypes = subclassesOf(classSymbol).map(subclassType(_, classSymbol.toType))
        val toType = subclassType(to, classSymbol.toType)
        State.traverse(subtypes) { subtype =>
          if (subtype <:< toType) {
            helpMergeOptics(to.asClass, tail).map(om => subtype.typeSymbol -> om)
          } else {
            State.getOrElseUpdate(subtype, c.freshName(subtype.typeSymbol.name).toTermName)
              .map { name =>
                subtype.typeSymbol -> NotSpecified(name)
              }
          }
        }.map(_.toMap).map(SpecifiedSealedTrait)
      case Selector(Lens(from, to, tpe), tail) =>
        State.sequence {
          paramListsOf(publicConstructor(classSymbol.toType), from).map { params =>
            State.traverse(params) { param =>
              if (param.asTerm.name == to) {
                helpMergeOptics(tpe.typeSymbol.asClass, tail).map(om =>
                  param.name.toTermName -> om
                )
              } else {
                val paramType = param.info
                State.getOrElseUpdate(paramType, c.freshName(paramType.typeSymbol.name).toTermName).map { name =>
                  param.name.toTermName -> NotSpecified(name)
                }
              }
            }.map(_.toMap)
          }
        }.map(SpecifiedCaseClass(classSymbol, _))
      case arg: Arg => State.pure(Specified(arg))
    }
  }

  def mergeMethods[A: WeakTypeTag](ast: Methods): VarsState[Option[SpecifiedGen]] = {
    State.traverse(ast) { selector =>
        helpMergeOptics(weakTypeOf[A].typeSymbol.asClass, selector)
    }.map { list =>
      list.foldLeft[Option[SpecifiedGen]](None) {
        case (Some(left), right) => Some(mergeSpecifications(left, right))
        case (None, value) => Some(value)
      }
    }
  }

  def mergeSpecifications(
    left: SpecifiedGen,
    right: SpecifiedGen
  ): SpecifiedGen = (left, right) match {
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

  def specifiedTree(gen: SpecifiedGen): c.Tree = gen match {
    case SpecifiedCaseClass(classSymbol, fields) =>
      val args = fields.map(_.values.map(specifiedTree).toList)
      construct(classSymbol.toType, args)
    case SpecifiedSealedTrait(subclasses) => constructCases(subclasses.values)(specifiedTree)
    case Specified(GenArg(tree)) => callApply(tree)
    case Specified(ConstArg(tree)) => tree
    case NotSpecified(tree)        => callApply(Ident(tree))
  }

  def toGen(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Gen.apply($tree)"

  def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Gen.const($tree)"

  def callApply(tree: c.Tree): c.Tree = q"$tree.apply()"

  def implicitly(tpe: c.Type): c.Tree = q"_root_.scala.Predef.implicitly[$tpe]"

  def constValueOf(tpe: c.Type): c.Tree = toConst(q"_root_.scala.Predef.valueOf[$tpe]")

  def constructType[F[_]](tpe: c.Type)(implicit weakTypeTag: WeakTypeTag[F[_]]): c.Type = {
    appliedType(weakTypeTag.tpe.typeConstructor, tpe)
  }

  def construct(tpe: c.Type, constructorArgs: List[List[c.Tree]]): c.Tree = {
    val constructionMethodTree: Tree = Select(New(Ident(tpe.dealias.typeSymbol)), termNames.CONSTRUCTOR)
    constructorArgs.foldLeft(constructionMethodTree)(Apply.apply)
  }

  def constructCases[T](cases: Iterable[T])(f: T => c.Tree): c.Tree = {
    q"_root_.scala.util.Random.nextInt(${cases.size}) match { case ..${cases.zipWithIndex.map {
      case value -> index => cq"$index => ${f(value)}"
    }} }"
  }

  def subclassesOf(parent: ClassSymbol): Set[c.Symbol] = {
    val (abstractChildren, concreteChildren) =
      parent.knownDirectSubclasses
        .tapEach(_.info)
        .partition(_.isAbstract)

    concreteChildren.foreach { child =>
      if (!child.asClass.isFinal && !child.asClass.isCaseClass) {
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

  def publicConstructor(tpe: c.Type): MethodSymbol = {
    val members = tpe.members
    members
      .find(m => m.isMethod && m.asMethod.isPrimaryConstructor && m.isPublic)
      .orElse(
        members.find(m => m.isMethod && m.asMethod.isConstructor && m.isPublic)
      )
      .map(_.asMethod)
      .getOrElse {
        c.abort(
          c.enclosingPosition,
          s"class ${tpe.typeSymbol.asClass.name.decodedName} has no public constructors"
        )
      }
  }
}
