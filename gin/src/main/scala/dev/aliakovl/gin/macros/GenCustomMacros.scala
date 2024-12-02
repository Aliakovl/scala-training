package dev.aliakovl.gin.macros

import dev.aliakovl.gin.Gen
import dev.aliakovl.gin.macros.fp.data.State
import dev.aliakovl.gin.macros.fp.syntax._

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

private[macros] trait GenCustomMacros { self: StateMacros with CommonMacros =>
  val c: whitebox.Context

  import c.universe._

  private val genSymbol: Symbol = symbolOf[Gen.type].asClass.module
  private val ginModule: ModuleSymbol = typeOf[dev.aliakovl.gin.`package`.type].termSymbol.asModule

  def mkCustomValue(prefix: c.Tree, typeToGen: c.Type): VarsState[Option[c.Tree]] = {
    mergeMethods(parseMethods(prefix), typeToGen)
      .flatTap {
        _.traverse { gen =>
          State.modify[Variables](deleteUnused(gen, _, typeToGen))
        }
      }
      .map {
        _.map { gen =>
          withName { termName =>
            toGen(termName)(specifiedTree(termName)(gen))
          }
        }
      }
  }

  private type Methods = List[Method]
  private type Selector = List[Optic]

  private sealed trait Optic
  private case class Lens(fromType: c.Type, field: c.TermName, toType: c.Type) extends Optic
  private case class Prism(toType: c.Type) extends Optic

  private def focusWithPrism(tpe: c.Type, toType: c.Type)(next: VarsState[CustomRepr]): VarsState[CustomRepr] = {
    if (tpe.typeConstructor =:= toType.typeConstructor) {
      tpe.typeArgs zip toType.typeArgs foreach { case (s, t) =>
        if (!(s =:= t)) fail(s"Type arguments of $toType must not be narrowed, fix: $t -> $s")
      }
      next
    } else {
      subTypesOf(tpe).traverse { subtype =>
        if (subtype.typeConstructor <:< toType.typeConstructor) {
          if (!(subtype <:< toType)) {
            fail(s"Type arguments of $toType must not be narrowed")
          }
          next.map(subtype -> _)
        } else {
          State.getOrElseUpdate(subtype.wrap, c.freshName(subtype.typeSymbol.name).toTermName)
            .map { name =>
              subtype -> NotSpecified(name)
            }
        }
      }.map(_.toMap).map(SpecifiedSealedTrait)
    }
  }

  private def focusWithLens(tpe: c.Type, fromType: c.Type, field: c.TermName)(next: c.Symbol => VarsState[CustomRepr]): VarsState[CustomRepr] = {
    val allParams = paramListsOf(publicConstructor(tpe), fromType)
    if (!allParams.flatten.map(_.name).contains(field)) c.abort(fromType.termSymbol.pos, s"Constructor of $tpe does not take $field argument")
    allParams.traverse { params =>
      params.traverse { param =>
        val termName = param.name.toTermName
        if (param.asTerm.name == field) {
          next(param).map(termName -> _)
        } else if (param.isImplicit) {
          val impl = c.inferImplicitValue(param.info)
          if (impl == EmptyTree) fail(s"Could not find implicit value for parameter ${param.name}: ${param.info.typeSymbol.name}")
          State.pure[Variables, CustomRepr](NotSpecifiedImplicit(impl)).map(termName -> _)
        } else {
          val paramType = param.info
          State.getOrElseUpdate(paramType.wrap, c.freshName(paramType.typeSymbol.name).toTermName).map { name =>
            param.name.toTermName -> NotSpecified(name)
          }
        }
      }.map(_.toMap)
    }.map(SpecifiedCaseClass(tpe, _))
  }

  private sealed trait Method {
    def toSpecifiedGen(tpe: c.Type): VarsState[CustomRepr]
  }

  private case class UseDefaultMethod(selector: Selector, pos: c.Position) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[CustomRepr] = toSpecifiedGen(tpe, selector)

    private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[CustomRepr] = optics match {
      case Lens(fromType, field, _) :: Nil =>
        val allParams = paramListsOf(publicConstructor(tpe), fromType)
        val defaultMap = defaults(patchedCompanionSymbolOf(tpe.typeSymbol))(allParams)
        focusWithLens(tpe, fromType, field) { param =>
          defaultMap.get(param).fold(fail(s"Constructor of $tpe parameter $field does not have default argument")) { default =>
            State.pure(Specified(DefaultArg(default), pos))
          }
        }
      case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
      case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
      case Nil => fail(s"Path in .useDefault(...) must end with constructor argument, not with .when[...]")
    }
  }

  private case class ExcludeMethod(selector: Selector, pos: c.Position) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[CustomRepr] = toSpecifiedGen(tpe, selector)

    private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[CustomRepr] = optics match {
      case Nil => State.pure(Excluded(pos))
      case Lens(_, field, _) :: Nil => fail(s"Path in .exclude(...) must end with .when[...], not with argument $field")
      case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
      case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
    }
  }

  private case class SpecifyMethod(selector: Selector, arg: Arg, pos: c.Position) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[CustomRepr] = toSpecifiedGen(tpe, selector)

    private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[CustomRepr] = optics match {
      case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
      case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
      case Nil => State.pure(Specified(arg, pos))
    }
  }

  private sealed trait Arg
  private case class GenArg(tree: c.Tree) extends Arg
  private case class ConstArg(tree: c.Tree) extends Arg
  private case class DefaultArg(tree: c.Tree) extends Arg

  private def mkPos(whole: c.Tree, other: c.Tree): c.Position = whole.pos.withStart(other.pos.end)

  private object SpecifyRef {
    def unapply(tree: c.Tree): Option[(SpecifyMethod, c.Tree)] = tree match {
      case q"$other.specify[$_](($_) => ${SelectorRef(selector)})($arg)" =>
        Some((SpecifyMethod(selector, GenArg(arg), mkPos(tree, other)), other))
      case _ => None
    }
  }

  private object SpecifyConstRef {
    def unapply(tree: c.Tree): Option[(SpecifyMethod, c.Tree)] = tree match {
      case q"$other.specifyConst[$_](($_) => ${SelectorRef(selector)})($arg)" =>
        Some((SpecifyMethod(selector, ConstArg(arg), mkPos(tree, other)), other))
      case _ => None
    }
  }

  private object UseDefaultRef {
    def unapply(tree: c.Tree): Option[(UseDefaultMethod, c.Tree)] = tree match {
      case q"$other.useDefault[$_](($_) => ${SelectorRef(selector)})" =>
        Some((UseDefaultMethod(selector, mkPos(tree, other)), other))
      case _ => None
    }
  }

  private object ExcludeRef {
    def unapply(tree: c.Tree): Option[(ExcludeMethod, c.Tree)] = tree match {
      case q"$other.exclude[$_](($_) => ${SelectorRef(selector)})" =>
        Some((ExcludeMethod(selector, mkPos(tree, other)), other))
      case _ => None
    }
  }

  private object CustomRef {
    def unapply(tree: c.Tree): Option[TypeSymbol] = tree match {
      case q"$module.custom[$tpe]" if module.symbol == genSymbol => Some(tpe.symbol.asType)
      case _ => None
    }
  }

  @tailrec
  private def parseMethods(tree: c.Tree, methods: Methods = List.empty): Methods = {
    tree match {
      case SpecifyRef(method, other) => parseMethods(other, method +: methods)
      case SpecifyConstRef(method, other) => parseMethods(other, method +: methods)
      case UseDefaultRef(method, other) => parseMethods(other, method +: methods)
      case ExcludeRef(method, other) => parseMethods(other, method +: methods)
      case CustomRef(_) => methods
      case _ => c.abort(tree.pos, "Unsupported syntax")
    }
  }

  private object LensRef {
    def unapply(tree: c.Tree): Option[(Lens, c.Tree)] = tree match {
      case q"$other.$field" =>
        val lens = Lens(other.tpe.dealias, field, tree.tpe.widen.dealias)
        Some((lens, other))
      case _ => None
    }
  }

  private object ArgLensRef {
    def unapply(tree: c.Tree): Option[(Lens, c.Tree)] = tree match {
      case q"$module.GenCustomOps[$_]($other).arg[$_]($fieldName)" if module.symbol == ginModule =>
        fieldName match {
          case Literal(Constant(name: String)) =>
            val lens = Lens(other.tpe.dealias, TermName(name), tree.tpe.widen.dealias)
            Some((lens, other))
          case _ => c.abort(fieldName.pos, "Only string literals supported")
        }
      case _ => None
    }
  }

  private object PrismRef {
    def unapply(tree: c.Tree): Option[(Prism, c.Tree)] = tree match {
      case q"$module.GenCustomOps[$from]($other).when[$to]" if module.symbol == ginModule =>
        if (from.symbol.isAbstract && !from.symbol.asClass.isSealed) c.abort(to.pos, s"Type $from is not sealed")
        val prism = Prism(to.tpe.dealias)
        Some((prism, other))
      case _ => None
    }
  }

  private object SpecialPrismRef {
    def unapply(tree: c.Tree): Option[(Prism, c.Tree)] = tree match {
      case q"$_[$from]($other).when[$to](..$_)" =>
        if (from.symbol.isAbstract && !from.symbol.asClass.isSealed) c.abort(to.pos, s"Type $from is not sealed")
        val toType = subclassType(to.symbol, from.tpe.dealias)
        val prism = Prism(toType)
        Some((prism, other))
      case _ => None
    }
  }

  private object SelectorRef {
    def unapply(tree: c.Tree): Option[Selector] = Some(parseSelector(tree))
  }

  @tailrec
  private def parseSelector(tree: c.Tree, selector: Selector = List.empty): Selector = {
    tree match {
      case LensRef(lens, other) => parseSelector(other, lens :: selector)
      case ArgLensRef(lens, other) => parseSelector(other, lens :: selector)
      case PrismRef(prism, other) => parseSelector(other, prism :: selector)
      case SpecialPrismRef(prism, other) => parseSelector(other, prism :: selector)
      case _: Ident => selector
      case tree => c.abort(tree.pos, "Unsupported path element")
    }
  }

  private trait HasPosition {
    def pos: c.Position
  }

  private sealed trait CustomRepr

  private sealed trait OpticRepr extends CustomRepr
  private case class SpecifiedCaseClass(tpe: c.Type, fields: List[Map[c.TermName, CustomRepr]]) extends OpticRepr
  private case class SpecifiedSealedTrait(subclasses: Map[c.Type, CustomRepr]) extends OpticRepr

  private sealed trait SpecifiedRepr extends CustomRepr with HasPosition
  private case class Specified(tree: Arg, pos: c.Position) extends SpecifiedRepr
  private object Specified {
    def unapply(s: Specified): Option[Arg] = Some(s.tree)
  }
  private case class Excluded(pos: c.Position) extends SpecifiedRepr

  private sealed trait NotSpecifiedRepr extends CustomRepr
  private case class NotSpecifiedImplicit(tree: c.Tree) extends NotSpecifiedRepr
  private case class NotSpecified(name: c.TermName) extends NotSpecifiedRepr

  private def usedVariables(gen: CustomRepr): Set[c.TermName] = gen match {
    case SpecifiedCaseClass(_, fields) => fields.flatten.flatMap { case (name, field) =>
      usedVariables(field) + name
    }.toSet
    case SpecifiedSealedTrait(subclasses) => subclasses.values.toSet.flatMap(usedVariables)
    case NotSpecified(tree) => Set(tree)
    case _ => Set.empty
  }

  private def deleteUnused(gen: CustomRepr, variables: Variables, typeToGen: c.Type): Variables = {
    val used = usedVariables(gen)
    variables.filter { case (tpe, name) =>
      used.contains(name) || tpe.tpe =:= typeToGen
    }
  }

  private def mergeMethods(methods: Methods, typeToGen: c.Type): VarsState[Option[CustomRepr]] = {
    methods.traverse(_.toSpecifiedGen(typeToGen)).map { representations =>
      representations.reduceOption {
        mergeSpecifications(_, _)
          .getOrElse(fail(printConflicts(aggregateConflicts(representations))))
      }
    }
  }

  private case class Conflict(pos: c.Position, withPos: c.Position) {
    def asString(i: Int): String = {
      s"""$i. ${pos.source.path}:${pos.line}
         |${showPos(pos)}
         |with ${withPos.source.path}:${withPos.line}
         |${showPos(withPos)}
         |""".stripMargin
    }
  }

  private def printConflicts(conflicts: List[Conflict]): String = {
    conflicts.zipWithIndex.map { case (conflict, ind) =>
      conflict.asString(ind + 1)
    }.mkString("Conflicts:\n", "", "")
  }

  private def aggregateConflicts(representations: List[CustomRepr]): List[Conflict] = {
    representations.foldLeft((Set.empty[CustomRepr], List.empty[Conflict])) { case ((prevs, acc), repr) =>
      val res = prevs.map(mergeSpecifications(_, repr)).collect {
        case Left(value) => value
      }
      (prevs + repr, acc ++ res)
    }._2
  }

  private def mergeSpecifications(
    left: CustomRepr,
    right: CustomRepr
  ): Either[Conflict, CustomRepr] = (left, right) match {
    case (SpecifiedCaseClass(_, leftFields), SpecifiedCaseClass(rightClass, rightFields)) =>
      leftFields.zip(rightFields).traverse { case (leftArgs, rightArgs) =>
        leftArgs.iterator.traverse { case (key, leftArg) =>
          mergeSpecifications(leftArg, rightArgs(key)).map(key -> _)
        }.map(_.toMap)
      }.map(SpecifiedCaseClass(rightClass, _))
    case (SpecifiedSealedTrait(leftSubclasses), SpecifiedSealedTrait(rightSubclasses)) =>
      leftSubclasses.iterator.traverse { case (key, left) =>
        mergeSpecifications(left, rightSubclasses(key)).map(key -> _)
      }.map(subclasses => SpecifiedSealedTrait(subclasses.toMap))
    case (SpecifiedSealedTrait(subclasses), right: SpecifiedCaseClass) =>
      subclasses.iterator.traverse { case (subclass, leftSpecified) =>
        mergeSpecifications(leftSpecified, right).map(subclass -> _)
      }.map(x => SpecifiedSealedTrait(x.toMap))
    case (left: SpecifiedCaseClass, SpecifiedSealedTrait(subclasses)) =>
      subclasses.iterator.traverse { case (subclass, rightSpecified) =>
        mergeSpecifications(left, rightSpecified).map(subclass -> _)
      }.map(x => SpecifiedSealedTrait(x.toMap))
    case (left, _: NotSpecifiedRepr) => Right(left)
    case (_: NotSpecifiedRepr, right) => Right(right)
    case (l: SpecifiedRepr, r: SpecifiedRepr) => Left(Conflict(l.pos, r.pos))
    case (l: SpecifiedRepr, r: OpticRepr) => Left(Conflict(l.pos, getPositions(r).head))
    case (l: OpticRepr, r: SpecifiedRepr) => Left(Conflict(getPositions(l).head, r.pos))
  }

  private def getPositions(customRepr: CustomRepr): List[c.Position] = {
    customRepr match {
      case SpecifiedCaseClass(_, fields) => fields.flatMap(_.values.flatMap(getPositions))
      case SpecifiedSealedTrait(subclasses) => subclasses.values.flatMap(getPositions).toList
      case repr: SpecifiedRepr => List(repr.pos)
      case _: NotSpecifiedRepr => List()
    }
  }

  private def specifiedTree(termName: c.TermName)(gen: CustomRepr): c.Tree = gen match {
    case SpecifiedCaseClass(classSymbol, fields) =>
      val args = fields.foldLeft(List.empty[List[c.Tree]]) { case (acc, next) =>
        val params = next.values.map {
          case Specified(DefaultArg(value)) => q"$value(...$acc)"
          case other => specifiedTree(termName)(other)
        }.toList
        acc :+ params
      }
      construct(classSymbol, args)
    case SpecifiedSealedTrait(subclasses) =>
      val cases = subclasses.filterNot(_._2.isInstanceOf[Excluded])
      if (cases.isEmpty) fail("All subtypes was excluded")
      constructCases(termName)(cases)(specifiedTree(termName))
    case Specified(GenArg(tree)) => callApply(tree)(termName)
    case Specified(ConstArg(tree)) => tree
    case NotSpecified(name) => callApply(Ident(name))(termName)
    case NotSpecifiedImplicit(tree) => tree
    case Excluded(_) => fail("All subtypes was excluded")
    case _ => fail("Unreachable")
  }
}
