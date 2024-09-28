package dev.aliakovl.gin.macros

import cats.implicits.toTraverseOps
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

  def fail(message: String): Nothing = c.abort(c.enclosingPosition, message)

  def initVars[A: c.WeakTypeTag]: Variables = Map(
    weakTypeOf[A] -> c.freshName(weakTypeOf[A].typeSymbol.name).toTermName
  )

  def materializeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = {
    val (variables, values) = initValues[A](None).run(initVars[A])

    c.Expr[Gen[A]] {
      genTree[A](variables, values)
    }
  }

  def makeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = {
    val (variables, values) = State.sequence {
        Option.when(weakTypeOf[A].typeSymbol.isClass) {
          val termName = TermName(c.freshName())
          mergeMethods[A](disassembleTree(c.prefix.tree))
            .map(_.map(specifiedTree(termName)).map(toGen(termName)))
        }
      }
      .map(_.flatten)
      .flatMap(initValues[A])
      .run(initVars[A])

    c.Expr[Gen[A]] {
      genTree[A](variables, values)
    }
  }

  sealed trait Value
  case class Refer(value: c.Tree) extends Value
  case class CaseClass(fields: List[List[c.TermName]]) extends Value
  case object CaseObject extends Value
  case class SealedTrait(subclasses: Map[c.Type, c.TermName]) extends Value

  def genTree[A: c.WeakTypeTag](variables: Variables, values: Values): c.Tree = {
    def lazyVal(tpe: c.Type, value: c.Tree): c.Tree = {
      q"lazy val ${variables(tpe)}: _root_.dev.aliakovl.gin.Gen[$tpe] = $value"
    }

    val declaration = values.map {
      case tp -> Refer(value) => lazyVal(tp, c.untypecheck(value.duplicate))
      case tp -> CaseObject => lazyVal(tp, constValueOf(tp))
      case tp -> CaseClass(fields) =>
        val termName = TermName(c.freshName())
        val args = fields.map(_.map(name => callApply(Ident(name))(termName)))
        construct(tp, args)
        val value = toGen(termName)(construct(tp, args))
        lazyVal(tp, value)
      case tp -> SealedTrait(subclasses) =>
        if (subclasses.isEmpty) fail(s"Type ${tp.typeSymbol.name} does not have constructors")
        val termName = TermName(c.freshName())
        val value = toGen(termName)(constructCases(termName)(subclasses){ name => callApply(Ident(name))(termName) })
        lazyVal(tp, value)
    }

    q"{..$declaration; ${variables(weakTypeOf[A])}}"
  }

  def buildValue[A: c.WeakTypeTag](tpe: c.Type): FullState[Value] = {
    val sym = tpe.typeSymbol
    val genType = constructType[Gen](tpe)
    val implicitValue = c.inferImplicitValue(genType, withMacrosDisabled = true)
    if (implicitValue != EmptyTree && tpe != weakTypeOf[A]) {
      State.pure(Refer(implicitValue))
    } else if (c.inferImplicitValue(constructType[ValueOf](tpe), withMacrosDisabled = true).nonEmpty) {
      State.pure(CaseObject)
    } else if (sym.isAbstract && sym.isClass && !sym.asClass.isSealed) {
      c.abort(c.enclosingPosition, s"Can not build Gen[$tpe], because abstract type $tpe is not sealed. Try to provide it implicitly")
    } else if (isAbstractSealed(sym)) {
      val subtypes = subTypesOf(tpe)
      State.traverse(subtypes) { subtype =>
        for {
          variables <- State.get[VState].map(_._1)
          name <- variables.get(subtype).fold {
              val termName = c.freshName(subtype.typeSymbol.name).toTermName
              State.modifyFirst[Variables, Values](_.updated(subtype, termName)).zip(getOrElseCreateValue(subtype)).as(termName)
            }(State.pure)
        } yield subtype -> name
      }.map(_.toMap).map(SealedTrait)
    } else if (isConcreteClass(sym)) {
      State.sequence {
        notImplicitParamLists(paramListsOf(publicConstructor(tpe), tpe)).map { params =>
          State.traverse(params) { param =>
            for {
              variables <- State.get[VState].map(_._1)
              paramType = param.info
              name <- variables.get(paramType).fold {
                  val termName = c.freshName(paramType.typeSymbol.name).toTermName
                  State.modifyFirst[Variables, Values](_.updated(paramType, termName)).zip(getOrElseCreateValue(paramType)).as(termName)
                }(State.pure)
            } yield name
          }
        }
      }.map(CaseClass)
    } else {
      c.abort(c.enclosingPosition, s"Can not build Gen[$tpe], try provide it implicitly")
    }
  }

  def getOrElseCreateValue[A: WeakTypeTag](tpe: c.Type): FullState[Value] = {
    State.get[VState].map(_._2).flatMap { values =>
      values.get(tpe) match {
        case Some(value) => State.pure(value)
        case None =>
          for {
            value <- buildValue[A](tpe)
            _ <- State.modifySecond[Variables, Values](_.updated(tpe, value))
          } yield value
      }
    }
  }

  def initValues[A: c.WeakTypeTag](tree: Option[c.Tree]): VarsState[Values] = {
    tree.fold {
        getOrElseCreateValue(weakTypeOf[A]).flatMap { value =>
          State.modifySecond[Variables, Values](_.updated(weakTypeOf[A], value))
        }
      } { value =>
        for {
          _ <- State.modifySecond[Variables, Values](
            _.updated(weakTypeOf[A], Refer(value))
          )
          variables <- State.get[VState].map(_._1)
          _ <- State.traverse(variables.keySet - weakTypeOf[A])(getOrElseCreateValue)
        } yield ()
      }
      .flatMap(_ => State.get[VState].map(_._2))
      .modifyState(_._1)((_, Map.empty))
  }

  type Methods = List[Method]
  type Selector = List[Optic]

  sealed trait Optic
  case class Lens(fromType: c.Type, field: c.TermName, toType: c.Type) extends Optic
  case class Prism(toType: c.Type) extends Optic

  sealed trait Method {
    def toSpecifiedGen(classSymbol: c.Type): VarsState[SpecifiedGen]
  }
  case class ExcludeMethod(excludedType: c.Type) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[SpecifiedGen] = {
      if (!tpe.typeSymbol.asClass.isSealed || !tpe.typeSymbol.isAbstract) fail(s"type $tpe is not abstract sealed")
      val subtypes = subTypesOf(tpe)
      State.traverse(subtypes.filterNot(_ <:< excludedType)) { subtype =>
        State.getOrElseUpdate(subtype, c.freshName(subtype.typeSymbol.name).toTermName)
          .map { name =>
            subtype -> NotSpecified(name)
          }
      }.map(_.toMap).map(SpecifiedSealedTrait)
    }
  }
  case class SpecifyMethod(selector: Selector, arg: Arg) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[SpecifiedGen] = toSpecifiedGen(tpe, selector)

    private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[SpecifiedGen] = optics match {
      case Prism(toType) :: tail =>
        if (tpe =:= toType) {
          toSpecifiedGen(toType, tail)
        } else {
          val subtypes = subTypesOf(tpe)
          State.traverse(subtypes) { subtype =>
            if (subtype <:< toType) {
              toSpecifiedGen(toType, tail).map(subtype -> _)
            } else {
              State.getOrElseUpdate(subtype, c.freshName(subtype.typeSymbol.name).toTermName)
                .map { name =>
                  subtype -> NotSpecified(name)
                }
            }
          }.map(_.toMap).map(SpecifiedSealedTrait)
        }
      case Lens(fromType, field, toType) :: tail =>
        State.sequence {
          val allParams: List[List[c.Symbol]] = paramListsOf(publicConstructor(tpe), fromType)
          val defaultMap = defaults(tpe.typeSymbol.asClass.companion)(allParams)
          if (!allParams.flatten.map(_.name).contains(field)) c.abort(fromType.termSymbol.pos, s"Constructor of $fromType does not take $field argument")
          allParams.map { params =>
            State.traverse(params) { param =>
              val termName = param.name.toTermName
              if (param.asTerm.name == field) {
                val defaultOpt = defaultMap.get(param)
                if (tail.isEmpty && arg.isInstanceOf[DefaultArg] && defaultOpt.nonEmpty) {
                  State.pure[Variables, SpecifiedGen](Specified(DefaultArg(defaultOpt))).map(termName -> _)
                } else {
                  toSpecifiedGen(toType, tail).map(termName -> _)
                }
              } else if (param.isImplicit) {
                val impl = c.inferImplicitValue(param.info)
                if (impl == EmptyTree) fail(s"could not find implicit value for parameter ${param.name}: ${param.info.typeSymbol.name}")
                State.pure[Variables, SpecifiedGen](NotSpecifiedImplicit(impl)).map(termName -> _)
              } else {
                val paramType = param.info
                State.getOrElseUpdate(paramType, c.freshName(paramType.typeSymbol.name).toTermName).map { name =>
                  param.name.toTermName -> NotSpecified(name)
                }
              }
            }.map(_.toMap)
          }
        }.map(SpecifiedCaseClass(tpe, _))
      case Nil => State.pure(Specified(arg))
    }
  }

  sealed trait Arg
  case class GenArg(tree: c.Tree) extends Arg
  case class ConstArg(tree: c.Tree) extends Arg
  case class DefaultArg(tree: Option[c.Tree]) extends Arg

  @tailrec
  final def disassembleTree[A: c.WeakTypeTag](tree: c.Tree, methods: Methods = List.empty): Methods = {
    tree match {
      case q"$other.specify[$tpe](($_) => $selectorTree)($arg)" =>
        val selector = disassembleSelector(selectorTree, tpe.tpe)
        val method = SpecifyMethod(selector, GenArg(arg))
        disassembleTree(other, method +: methods)
      case q"$other.specifyConst[$tpe](($_) => $selectorTree)($arg)" =>
        val selector = disassembleSelector(selectorTree, tpe.tpe)
        val method = SpecifyMethod(selector, ConstArg(arg))
        disassembleTree(other, method +: methods)
      case q"$other.useDefault[$tpe](($_) => $selectorTree)" =>
        val selector = disassembleSelector(selectorTree, tpe.tpe)
        if (selector.last.isInstanceOf[Prism]) {
          c.abort(selectorTree.pos, "Default value can be specified only for class constructor fields")
        }
        val method = SpecifyMethod(selector, DefaultArg(None))
        disassembleTree(other, method +: methods)
      case q"$other.exclude[$tpeTree]" =>
        val method = ExcludeMethod(tpeTree.tpe)
        disassembleTree(other, method +: methods)
      case q"$module.custom[$_]" if module.symbol == genSymbol => methods
      case _ => c.abort(tree.pos, "Unsupported syntax.")
    }
  }

  @tailrec
  final def disassembleSelector(tree: c.Tree, tpe: c.Type, selector: Selector = List.empty): Selector = {
    tree match {
      case q"$other.$field" =>
        val lens = Lens(other.tpe, field, tpe)
        disassembleSelector(other, other.tpe, lens :: selector)
      case q"$module.GenWhen[$from]($other).arg[$to]($fieldName)" if module.symbol == ginModule =>
        fieldName match {
          case Literal(Constant(name: String)) =>
            val lens = Lens(other.tpe, TermName(name), tree.tpe)
            disassembleSelector(other, from.tpe, lens :: selector)
          case _ => c.abort(fieldName.pos, "Only string literals supported")
        }
      case q"$module.GenWhen[$from]($other).when[$to]" if module.symbol == ginModule =>
        if (from.symbol.isAbstract && !from.symbol.asClass.isSealed) c.abort(to.pos ,s"$from is not sealed")
        val prism = Prism(to.tpe)
        disassembleSelector(other, from.tpe, prism :: selector)
      case _: Ident => selector
      case tree => c.abort(tree.pos, "Unsupported path element.")
    }
  }

  sealed trait SpecifiedGen
  case class SpecifiedCaseClass(sym: c.Type, fields: List[Map[c.TermName, SpecifiedGen]]) extends SpecifiedGen
  case class SpecifiedSealedTrait(subclasses: Map[c.Type, SpecifiedGen]) extends SpecifiedGen
  case class Specified(tree: Arg) extends SpecifiedGen
  case class NotSpecifiedImplicit(tree: c.Tree) extends SpecifiedGen
  case class NotSpecified(tree: c.TermName) extends SpecifiedGen

  def mergeMethods[A: WeakTypeTag](methods: Methods): VarsState[Option[SpecifiedGen]] = {
    State.traverse(methods)(_.toSpecifiedGen(weakTypeOf[A])).map { list =>
      list.foldLeft[Option[SpecifiedGen]](None) {
        case (Some(left), right) => Some(mergeSpecifications(left, right))
        case (None, value) => Some(value)
      }
    }
  }

  def join[K, A, B](left: Map[K, A], right: Map[K, B]): Map[K, (A, B)] = {
    (left.keySet & right.keySet).map { key =>
      key -> (left(key), right(key))
    }.toMap
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
      SpecifiedSealedTrait(join(leftSubclasses, rightSubclasses).map { case (key, (left, right)) =>
        key -> mergeSpecifications(left, right)
      })
    case (SpecifiedSealedTrait(subclasses), _: SpecifiedCaseClass) =>
      SpecifiedSealedTrait(subclasses.map { case (subclass, leftSpecified) =>
        subclass -> mergeSpecifications(leftSpecified, right)
      })
    case (_: SpecifiedCaseClass, SpecifiedSealedTrait(subclasses)) =>
      SpecifiedSealedTrait(subclasses.map { case (subclass, rightSpecified) =>
        subclass -> mergeSpecifications(left, rightSpecified)
      })
    case (SpecifiedCaseClass(_, fields), _: NotSpecified | _: NotSpecifiedImplicit) =>
      if (fields.nonEmpty) left else right
    case (SpecifiedSealedTrait(subclasses), _: NotSpecified) =>
      if (subclasses.nonEmpty) left else right
    case (_: NotSpecified | _: NotSpecifiedImplicit, SpecifiedCaseClass(_, fields)) =>
      if (fields.nonEmpty) right else left
    case (_: NotSpecified | _: NotSpecifiedImplicit, SpecifiedSealedTrait(subclasses)) =>
      if (subclasses.nonEmpty) right else left
    case (_: NotSpecified | _: NotSpecifiedImplicit, _: NotSpecified | _: NotSpecifiedImplicit) => right
    case (_: NotSpecified | _: NotSpecifiedImplicit, _: Specified) => right
    case (_: Specified, _: NotSpecified | _: NotSpecifiedImplicit) => left
    case _ => fail(s"Some specifications conflict")
  }

  def specifiedTree(termName: TermName)(gen: SpecifiedGen): c.Tree = gen match {
    case SpecifiedCaseClass(classSymbol, fields) =>
      val args = fields.foldLeft(List.empty[List[c.Tree]]) { case (acc, next) =>
        val params = next.values.map {
          case Specified(DefaultArg(Some(value))) => q"$value(...$acc)"
          case other => specifiedTree(termName)(other)
        }.toList

        acc :+ params
      }
      construct(classSymbol, args)
    case SpecifiedSealedTrait(subclasses) =>
      if (subclasses.isEmpty) fail("All subtypes was excluded")
      constructCases(termName)(subclasses)(specifiedTree(termName))
    case Specified(GenArg(tree)) => callApply(tree)(termName)
    case Specified(ConstArg(tree)) => tree
    case NotSpecified(tree)        => callApply(Ident(tree))(termName)
    case NotSpecifiedImplicit(tree)   => tree
    case _ => fail("unreachable")
  }

  def isAbstractSealed(sym: c.Symbol): Boolean = {
    sym.isAbstract && sym.isClass && sym.asClass.isSealed
  }

  def isConcreteClass(sym: c.Symbol): Boolean = {
    sym.isClass && !sym.isAbstract
  }

  def toGen(termName: TermName)(tree: c.Tree): c.Tree = {
    val f: c.Tree = Function(ValDef(Modifiers(Flag.PARAM), termName, TypeTree(), EmptyTree) :: Nil, tree)
    q"_root_.dev.aliakovl.gin.Gen.apply($f)"
  }

  def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Gen.const($tree)"

  def callApply(tree: c.Tree)(termName: TermName): c.Tree = q"$tree.apply(${Ident(termName)})"

  def implicitly(tpe: c.Type): c.Tree = q"_root_.scala.Predef.implicitly[$tpe]"

  def constValueOf(tpe: c.Type): c.Tree = toConst(q"_root_.scala.Predef.valueOf[$tpe]")

  def constructType[F[_]](tpe: c.Type)(implicit weakTypeTag: WeakTypeTag[F[_]]): c.Type = {
    appliedType(weakTypeTag.tpe.typeConstructor, tpe)
  }

  def construct(tpe: c.Type, constructorArgs: List[List[c.Tree]]): c.Tree = {
    val constructionMethodTree: Tree = Select(New(Ident(tpe.dealias.typeSymbol)), termNames.CONSTRUCTOR)
    constructorArgs.foldLeft(constructionMethodTree)(Apply.apply)
  }

  def constructCases[T](termName: TermName)(subclasses: Map[c.Type, T])(f: T => c.Tree): c.Tree = {
    val cases = subclasses.toList.sortBy(_._1.typeSymbol.fullName).map(_._2)
    q"$termName.nextInt(${cases.size}) match { case ..${cases.zipWithIndex.map {
      case value -> index => cq"$index => ${f(value)}"
    }} }"
  }

  def subTypesOf(parent: c.Type): Set[c.Type] = {
    val (abstractChildren, concreteChildren) = parent.typeSymbol.asClass.knownDirectSubclasses.tapEach(_.info)
      .partition(_.isAbstract)

    concreteChildren.foreach { child =>
      if (!child.asClass.isFinal && !child.asClass.isCaseClass) {
        fail(s"child $child of $parent is neither final nor a case class")
      }
    }

    concreteChildren.flatMap(subclassAllTypes(_, parent)) ++ abstractChildren.flatMap(subclassAllTypes(_, parent)).flatMap { child =>
      val childClass = child.typeSymbol.asClass
      if (childClass.isSealed) {
        subTypesOf(child)
      } else {
        fail(s"child $child of $parent is not sealed")
      }
    }
  }

  def subclassAllTypes(subclass: c.Symbol, parent: c.Type): Seq[c.Type] = {
    val sEta = subclass.asType.toType.etaExpand
    val subclassTypeArgs = sEta.baseType(parent.typeSymbol).typeArgs.map(_.typeSymbol)
    if (parent.typeArgs.isEmpty) {
      Seq(sEta)
    } else {
      val allCases = subclassTypeArgs.zip(parent.typeArgs).map { case (sym, tpe) =>
        if (sym.asType.isCovariant && tpe.typeSymbol.isClass) {
          val subTypes = subTypesOf(tpe).toList
          if (subTypes.nonEmpty) {
            subTypes
          } else {
            List(tpe)
          }
        } else {
          List(tpe)
        }
      }.sequence

      allCases.map { typeArgs =>
        sEta.finalResultType.substituteTypes(
          from = subclassTypeArgs,
          to = typeArgs
        )
      }
    }
  }

  def defaults(companion: c.Symbol)(params: List[List[c.Symbol]]): Map[c.Symbol, c.Tree] = {
    val res: List[List[Option[c.Tree]]] = params.foldLeft(List.empty[List[Option[c.Tree]]]) { case (acc, next) =>
      val defaultOpts: List[Option[c.Tree]] = next.zipWithIndex.map { case (sym, index) =>
        Option.when(sym.asTerm.isParamWithDefault) {
          val i = acc.flatten.size + index + 1
          val default = companion.info.member(TermName(s"<init>$$default$$$i").encodedName)
          q"$companion.$default"
        }
      }
      acc :+ defaultOpts
    }

    params.flatten.zip(res.flatten).collect {
      case sym -> Some(tree) => sym -> tree
    }.toMap
  }

  def paramListsOf(method: c.Symbol, tpe: c.Type): List[List[c.universe.Symbol]] = {
    method.asMethod.infoIn(tpe).paramLists
  }

  def notImplicitParamLists(params: List[List[c.universe.Symbol]]): List[List[c.universe.Symbol]] = {
    params.filterNot(_.headOption.exists(_.isImplicit))
  }

  def isPhantomConstructor(constructor: Symbol): Boolean = constructor.asMethod.fullName.endsWith("$init$")

  def publicConstructor(tpe: c.Type): MethodSymbol = {
    val members = tpe.members
    val constructors: Iterable[MethodSymbol] = members
      .filter(m => m.isMethod && m.asMethod.isConstructor && m.isPublic)
      .filterNot(isPhantomConstructor)
      .map(_.asMethod)

    constructors
      .find(_.isPrimaryConstructor)
      .orElse(constructors.headOption)
      .getOrElse(fail(s"class ${tpe.typeSymbol.name} has no public constructors"))
  }
}
