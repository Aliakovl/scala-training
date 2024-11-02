package dev.aliakovl.gin
package macros

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

class GenMacro(val c: whitebox.Context) {
  import c.universe._

  def materializeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = {
    val (variables, values) = initValues[A](None).run(initVars[A])

    genTree[A](variables, values)
  }

  def makeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = {
    val (variables, values) = State.sequence {
        Option.when(weakTypeOf[A].typeSymbol.isClass) {
          mergeMethods[A](disassembleTree(c.prefix.tree))
        }
      }
      .map(_.flatten)
      .flatTap { genOpt =>
        State.traverse(genOpt) { gen =>
          State.modify[Variables](deleteUnused(gen, _))
        }
      }
      .map { genOpt =>
        val termName = TermName(c.freshName())
        genOpt.map(specifiedTree(termName)).map(toGen(termName))
      }
      .flatMap(initValues[A])
      .run(initVars[A])

    genTree[A](variables, values)
  }

  type Variables = Map[c.Type, c.TermName]
  type Values = Map[c.Type, Value]
  type VarsState[A] = State[Variables, A]
  type VState = (Variables, Values)
  type FullState[A] = State[VState, A]

  val genSymbol = symbolOf[Gen.type].asClass.module
  val ginModule = c.mirror.staticModule("dev.aliakovl.gin.package")

  def fail(message: String): Nothing = c.abort(c.enclosingPosition, message)

  def initVars[A: c.WeakTypeTag]: Variables = Map(
    weakTypeOf[A] -> c.freshName(weakTypeOf[A].typeSymbol.name).toTermName
  )

  sealed trait Value
  case class Refer(value: c.Tree) extends Value
  case class CaseClass(fields: List[List[c.TermName]]) extends Value
  case object CaseObject extends Value
  case class SealedTrait(subclasses: Map[c.Type, c.TermName]) extends Value

  def block[A: c.WeakTypeTag](statements: List[c.Expr[Any]], expr: c.Expr[A]): c.Expr[A] = c.Expr[A](q"..$statements; $expr")

  def genTree[A: c.WeakTypeTag](variables: Variables, values: Values): c.Expr[Gen[A]] = {
    def lazyVal(variable: c.TermName, tpe: c.Type, value: c.Tree): c.Expr[Any] = c.Expr[Any] {
      q"lazy val $variable: _root_.dev.aliakovl.gin.Gen[$tpe] = $value"
    }

    def declaration(tpe: c.Type, value: Value): c.Tree = value match {
      case Refer(value) => value
      case CaseObject => constValueOf(tpe)
      case CaseClass(fields) =>
        val termName = TermName(c.freshName())
        val args = fields.map(_.map(name => callApply(Ident(name))(termName)))
        construct(tpe, args)
        toGen(termName)(construct(tpe, args))
      case SealedTrait(subclasses) =>
        if (subclasses.isEmpty) fail(s"Type ${tpe.typeSymbol.name} does not have constructors")
        val termName = TermName(c.freshName())
        toGen(termName)(constructCases(termName)(subclasses){ name => callApply(Ident(name))(termName) })
    }

    val declarations: List[c.Expr[Any]] = variables.map { case (tpe, variable) =>
      val value = values(tpe)
      lazyVal(variable, tpe, declaration(tpe, value))
    }.toList

    block(declarations, c.Expr[Gen[A]](Ident(variables(weakTypeOf[A]))))
  }

  def buildValue[A: c.WeakTypeTag](tpe: c.Type): FullState[Value] = {
    val sym = tpe.typeSymbol
    val genType = constructType[Gen](tpe)
    Option.when[c.Tree](tpe != weakTypeOf[A]) {
      c.inferImplicitValue(genType, withMacrosDisabled = true)
    }.flatMap { implicitValue =>
      Option.when[c.Tree](implicitValue != EmptyTree)(implicitValue)
    }.fold[FullState[Value]] {
      if (c.inferImplicitValue(constructType[ValueOf](tpe), withMacrosDisabled = true).nonEmpty) {
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
        c.abort(c.enclosingPosition, s"Can not build Gen[$tpe], try to provide it implicitly")
      }
    } { implicitValue =>
      State.pure(Refer(implicitValue))
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
            _.updated(weakTypeOf[A], Refer(c.untypecheck(value.duplicate)))
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

  def focusWithPrism(tpe : c.Type, toType: c.Type)(next: VarsState[SpecifiedGen]): VarsState[SpecifiedGen] = {
    if (tpe.typeConstructor =:= toType.typeConstructor) {
      tpe.typeArgs zip toType.typeArgs foreach { case (s, t) =>
        if (!(s =:= t)) fail(s"$toType type arguments must not be narrowed, fix: $t -> $s")
      }
      next
    } else {
      val subtypes = subTypesOf(tpe)
      State.traverse(subtypes) { subtype =>
        if (subtype.typeConstructor <:< toType.typeConstructor) {
          if (!(subtype <:< toType)) {
            fail(s"$toType type arguments must not be narrowed")
          }
          next.map(subtype -> _)
        } else {
          State.getOrElseUpdate(subtype, c.freshName(subtype.typeSymbol.name).toTermName)
            .map { name =>
              subtype -> NotSpecified(name)
            }
        }
      }.map(_.toMap).map(SpecifiedSealedTrait)
    }
  }

  def focusWithLens(tpe: c.Type, fromType: c.Type, field: c.TermName)(next: c.Symbol => VarsState[SpecifiedGen]): VarsState[SpecifiedGen] = {
    val allParams = paramListsOf(publicConstructor(tpe), fromType)
    if (!allParams.flatten.map(_.name).contains(field)) c.abort(fromType.termSymbol.pos, s"Constructor of $tpe does not take $field argument")
    State.sequence {
      allParams.map { params =>
        State.traverse(params) { param =>
          val termName = param.name.toTermName
          if (param.asTerm.name == field) {
            next(param).map(termName -> _)
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
  }

  sealed trait Method {
    def toSpecifiedGen(tpe: c.Type): VarsState[SpecifiedGen]
  }

  case class UseDefaultMethod(selector: Selector) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[SpecifiedGen] = toSpecifiedGen(tpe, selector)

    private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[SpecifiedGen] = optics match {
      case Lens(fromType, field, _) :: Nil =>
        val allParams = paramListsOf(publicConstructor(tpe), fromType)
        val defaultMap = defaults(patchedCompanionSymbolOf(tpe.typeSymbol))(allParams)
        focusWithLens(tpe, fromType, field) { param =>
          defaultMap.get(param).fold(fail(s"$field does not have default argument")) { default =>
            State.pure(Specified(DefaultArg(default)))
          }
        }
      case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
      case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
      case Nil => fail(s"Path in .useDefault(...) must end with constructor argument, not with .when[...]")
    }
  }

  case class ExcludeMethod(selector: Selector) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[SpecifiedGen] = toSpecifiedGen(tpe, selector)

    private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[SpecifiedGen] = optics match {
      case Nil => State.pure(Excluded)
      case Lens(_, field, _) :: Nil => fail(s"Path in .exclude(...) must end with .when[...], not with argument $field")
      case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
      case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
    }
  }

  case class SpecifyMethod(selector: Selector, arg: Arg) extends Method {
    override def toSpecifiedGen(tpe: c.Type): VarsState[SpecifiedGen] = toSpecifiedGen(tpe, selector)

    private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[SpecifiedGen] = optics match {
      case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
      case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
      case Nil => State.pure(Specified(arg))
    }
  }

  sealed trait Arg
  case class GenArg(tree: c.Tree) extends Arg
  case class ConstArg(tree: c.Tree) extends Arg
  case class DefaultArg(tree: c.Tree) extends Arg

  @tailrec
  final def disassembleTree[A: c.WeakTypeTag](tree: c.Tree, methods: Methods = List.empty): Methods = {
    tree match {
      case q"$other.specify[$_](($_) => $selectorTree)($arg)" =>
        val selector = disassembleSelector(selectorTree)
        val method = SpecifyMethod(selector, GenArg(arg))
        disassembleTree(other, method +: methods)
      case q"$other.specifyConst[$_](($_) => $selectorTree)($arg)" =>
        val selector = disassembleSelector(selectorTree)
        val method = SpecifyMethod(selector, ConstArg(arg))
        disassembleTree(other, method +: methods)
      case q"$other.useDefault[$_](($_) => $selectorTree)" =>
        val selector = disassembleSelector(selectorTree)
        val method = UseDefaultMethod(selector)
        disassembleTree(other, method +: methods)
      case q"$other.exclude[$_](($_) => $selectorTree)" =>
        val selector = disassembleSelector(selectorTree)
        val method = ExcludeMethod(selector)
        disassembleTree(other, method +: methods)
      case q"$module.custom[$_]" if module.symbol == genSymbol => methods
      case _ => c.abort(tree.pos, "Unsupported syntax.")
    }
  }

  @tailrec
  final def disassembleSelector(tree: c.Tree, selector: Selector = List.empty): Selector = {
    tree match {
      case q"$other.$field" =>
        val lens = Lens(other.tpe.dealias, field, tree.tpe.widen.dealias)
        disassembleSelector(other, lens :: selector)
      case q"$module.GenCustomOps[$_]($other).arg[$_]($fieldName)" if module.symbol == ginModule =>
        fieldName match {
          case Literal(Constant(name: String)) =>
            val lens = Lens(other.tpe.dealias, TermName(name), tree.tpe.widen.dealias)
            disassembleSelector(other, lens :: selector)
          case _ => c.abort(fieldName.pos, "Only string literals supported")
        }
      case q"$module.GenCustomOps[$from]($other).when[$to]" if module.symbol == ginModule =>
        if (from.symbol.isAbstract && !from.symbol.asClass.isSealed) c.abort(to.pos ,s"$from is not sealed")
        val prism = Prism(to.tpe)
        disassembleSelector(other, prism :: selector)
      case q"$_[$from]($other).when[$to](..$_)" =>
        if (from.symbol.isAbstract && !from.symbol.asClass.isSealed) c.abort(to.pos ,s"$from is not sealed")
        val toType = subclassType(to.symbol, from.tpe)
        val prism = Prism(toType)
        disassembleSelector(other, prism :: selector)
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
  case object Excluded extends SpecifiedGen

  def usedVariables(gen: SpecifiedGen): Set[c.TermName] = gen match {
    case SpecifiedCaseClass(_, fields) => fields.flatten.flatMap { case (name, field) =>
      usedVariables(field) + name
    }.toSet
    case SpecifiedSealedTrait(subclasses) => subclasses.values.toSet.flatMap(usedVariables)
    case NotSpecified(tree) => Set(tree)
    case _ => Set.empty
  }

  def deleteUnused[A: WeakTypeTag](gen: SpecifiedGen, variables: Variables): Variables = {
    val tpeA = weakTypeOf[A]
    val used = usedVariables(gen)
    variables.filter { case (tpe, name) =>
      used.contains(name) || tpe == tpeA
    }
  }

  def mergeMethods[A: WeakTypeTag](methods: Methods): VarsState[Option[SpecifiedGen]] = {
    State.traverse(methods)(_.toSpecifiedGen(weakTypeOf[A])).map { list =>
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
      SpecifiedSealedTrait(leftSubclasses.map { case (key, left) =>
        key -> mergeSpecifications(left, rightSubclasses(key))
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
    case (Excluded, _: NotSpecified | _: NotSpecifiedImplicit) => Excluded
    case (_: NotSpecified | _: NotSpecifiedImplicit, Excluded) => Excluded
    case _ => fail(s"Some specifications conflict")
  }

  def specifiedTree(termName: TermName)(gen: SpecifiedGen): c.Tree = gen match {
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
      val cases = subclasses.filterNot(_._2 == Excluded)
      if (cases.isEmpty) fail("All subtypes was excluded")
      constructCases(termName)(cases)(specifiedTree(termName))
    case Specified(GenArg(tree)) => callApply(tree)(termName)
    case Specified(ConstArg(tree)) => tree
    case NotSpecified(tree)        => callApply(Ident(tree))(termName)
    case NotSpecifiedImplicit(tree)   => tree
    case Excluded => fail("All subtypes was excluded")
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
    cases match {
      case singleCase :: Nil => f(singleCase)
      case _ => q"$termName.nextInt(${cases.size}) match { case ..${cases.zipWithIndex.map {
        case value -> index => cq"$index => ${f(value)}"
      }} }"
    }
  }

  def subTypesOf(parent: c.Type): Set[c.Type] =
    subclassesOf(parent.typeSymbol.asClass).map(subclassType(_, parent))

  def subclassesOf(parent: ClassSymbol): Set[c.Symbol] = {
    val (abstractChildren, concreteChildren) =
      parent.knownDirectSubclasses
        .tapEach(_.info)
        .partition(_.isAbstract)

    concreteChildren.foreach { child =>
      if (!child.asClass.isFinal && !child.asClass.isCaseClass && !child.isModuleClass) {
        fail(s"child $child of $parent is neither final nor a case class")
      }
    }

    concreteChildren ++ abstractChildren.flatMap { child =>
      val childClass = child.asClass
      if (childClass.isSealed) {
        subclassesOf(childClass)
      } else {
        fail(s"child $child of $parent is not sealed")
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

  def subclassType(subclass: c.Type, parent: c.Type, typeArgs: List[c.Type]): c.Type = {
    val sEta = subclass.etaExpand
    sEta.finalResultType.substituteTypes(
      from = sEta
        .baseType(parent.typeSymbol)
        .typeArgs
        .map(_.typeSymbol),
      to = typeArgs
    )
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

  def paramListsOf(method: c.Symbol, tpe: c.Type): List[List[c.Symbol]] = {
    method.asMethod.infoIn(tpe).paramLists
  }

  def notImplicitParamLists(params: List[List[c.Symbol]]): List[List[c.Symbol]] = {
    params.filterNot(_.headOption.exists(_.isImplicit))
  }

  def isPhantomConstructor(constructor: c.Symbol): Boolean = constructor.asMethod.fullName.endsWith("$init$")

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

  // https://github.com/scalamacros/paradise/blob/c14c634923313dd03f4f483be3d7782a9b56de0e/plugin/src/main/scala/org/scalamacros/paradise/typechecker/Namers.scala#L568-L613
  def patchedCompanionSymbolOf(original: c.Symbol): c.Symbol = {

    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val ctx = typer.context
    val owner = original.owner

    import global.analyzer.Context

    original.companion orElse {
      import global._
      implicit class PatchedContext(ctx: Context) {
        trait PatchedLookupResult { def suchThat(criterion: Symbol => Boolean): Symbol }
        def patchedLookup(name: Name, expectedOwner: Symbol) = new PatchedLookupResult {
          override def suchThat(criterion: Symbol => Boolean): Symbol = {
            var res: Symbol = NoSymbol
            var ctx = PatchedContext.this.ctx
            while (res == NoSymbol && ctx.outer != ctx) {
              val s = {
                val lookupResult = ctx.scope.lookupAll(name).filter(criterion).toList
                lookupResult match {
                  case Nil => NoSymbol
                  case List(unique) => unique
                  case _ => abort(s"unexpected multiple results for a companion symbol lookup for $original#{$original.id}")
                }
              }
              if (s != NoSymbol && s.owner == expectedOwner)
                res = s
              else
                ctx = ctx.outer
            }
            res
          }
        }
      }
      ctx.patchedLookup(original.asInstanceOf[global.Symbol].name.companionName, owner.asInstanceOf[global.Symbol]).suchThat(sym =>
        (original.isTerm || sym.hasModuleFlag) &&
          (sym isCoDefinedWith original.asInstanceOf[global.Symbol])
      ).asInstanceOf[c.Symbol]
    }
  }
}
