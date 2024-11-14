package dev.aliakovl.gin
package macros

import dev.aliakovl.gin.macros.State._
import dev.aliakovl.gin.macros.fp.syntax._

import scala.annotation.tailrec
import scala.reflect.internal.util.Position
import scala.reflect.macros.whitebox

object GenMacro {
  def makeImpl[A: c.WeakTypeTag](c: whitebox.Context): c.Expr[Gen[A]] = Stack.withContext[Gen[A]](c) { stack =>
    import c.universe._
    import stack._

    val typeToGen = weakTypeOf[A]
    val genSymbol = symbolOf[Gen.type].asClass.module
    val ginModule = c.mirror.staticModule("dev.aliakovl.gin.package")

    object LazyRef {
      private val symbol = symbolOf[Lazy.type].asClass.module

      def apply(tpe: c.Type, variable: String): c.Tree =
        q"$symbol.apply[$tpe]($variable)"

      def unapply(tree: c.Tree): Option[String] = tree match {
        case q"$module.apply[$_](${Literal(Constant(variable: String))})" if module.symbol == symbol => Some(variable)
        case _ => None
      }
    }

    val pullOutLazyVariables = new Transformer {
      override def transform(tre: c.Tree): c.Tree = tre match {
        case LazyRef(variable) => Ident(TermName(variable))
        case _                   => super.transform(tre)
      }
    }

    def fail(message: String): Nothing = c.abort(c.enclosingPosition, message)

    def withName[T](body: c.TermName => T): T = body(TermName(c.freshName()))

    def block[T](statements: List[c.Expr[Any]], expr: c.Expr[T]): c.Expr[T] =
      c.Expr[T](q"..$statements; $expr")

    def genTree(variables: Variables, values: Values): c.Expr[Gen[A]] = {
      def lazyVal(variable: c.TermName, tpe: c.Type, value: c.Tree): c.Expr[Any] = c.Expr[Any] {
        q"lazy val $variable: _root_.dev.aliakovl.gin.Gen[$tpe] = $value"
      }

      if (depth > 1) {
        withName { name =>
          c.Expr[Gen[A]](q"""{
            lazy val $name: ${weakTypeOf[Gen[A]]} = ${LazyRef(weakTypeOf[Gen[A]], variables(typeToGen).decodedName.toString)}
            $name
          }""")
        }
      } else {
        val declarations: List[c.Expr[Any]] = variables.map { case (tpe, variable) =>
          lazyVal(variable, tpe, values(tpe))
        }.toList
        block(declarations, c.Expr[Gen[A]](Ident(variables(typeToGen))))
      }
    }

    def buildValue(tpe: c.Type): FullState[c.Tree] = {
      val sym = tpe.typeSymbol
      if (c.inferImplicitValue(constructType[ValueOf](tpe), withMacrosDisabled = true).nonEmpty) {
        State.pure(constValueOf(tpe))
      } else if (sym.isAbstract && sym.isClass && !sym.asClass.isSealed) {
        c.abort(c.enclosingPosition, s"Can not build Gen[$tpe], because abstract type $tpe is not sealed. Try to provide it implicitly")
      } else if (isAbstractSealed(sym)) {
        val subtypes = subTypesOf(tpe)
        subtypes.traverse { subtype =>
          getVariableName(subtype).map(subtype -> _)
        }.map(_.toMap).map { subclasses =>
          if (subclasses.isEmpty) fail(s"Class ${tpe.typeSymbol.name} does not have constructors")
          withName { termName =>
            toGen(termName)(constructCases(termName)(subclasses) { name => callApply(Ident(name))(termName) })
          }
        }
      } else if (isConcreteClass(sym)) {
        notImplicitParamLists(paramListsOf(publicConstructor(tpe), tpe)).traverse { params =>
          params.traverse { param =>
            getVariableName(param.info)
          }
        }
        .map { fields =>
          withName { termName =>
            val args = fields.map(_.map(name => callApply(Ident(name))(termName)))
            toGen(termName)(construct(tpe, args))
          }
        }
      } else {
        c.abort(c.enclosingPosition, s"Can not build Gen[$tpe], try to provide it implicitly")
      }
    }

    def findImplicit(tpe: c.Type): FullState[c.Tree] = {
      val genType = constructType[Gen](tpe)
      statefulSearch {
        Option(c.inferImplicitValue(genType))
          .filterNot(_ == EmptyTree)
          .toRight(s"Fail to find implicit for type $tpe")
      }
        .map(_.fold(fail, identity))
        .map(tree => c.untypecheck(pullOutLazyVariables.transform(tree))) <* createIfNotExists(tpe)
    }

    def updateIfNotExists(tpe: c.Type, value: c.Tree): State[VState, Unit] = {
      for {
        values <- State.get[VState].map(_._2)
        _ <- if (values.contains(tpe)) {
          State.unit[VState]
        } else {
          State.modifySecond[Variables, Values](_.updated(tpe, value))
        }
      } yield ()
    }

    def createIfNotExists(tpe: c.Type): State[VState, c.TermName] = {
      for {
        variables <- State.get[VState].map(_._1)
        name <- variables.get(tpe) match {
          case Some(value) => State.pure[VState, c.TermName](value)
          case None =>
            val name = c.freshName(tpe.typeSymbol.name).toTermName
            State.modifyFirst[Variables, Values](_.updated(tpe, name)).as(name)
        }
      } yield name
    }

    def getVariableName(tpe: c.Type): FullState[TermName] = for {
      variables <- State.get[VState].map(_._1)
      name <- State.pure(variables.get(tpe)).fallback {
        findImplicit(tpe).flatMap(updateIfNotExists(tpe, _)) *> State.get[VState].map(_._1).map(_.apply(tpe))
      }
    } yield name

    def getOrElseCreateValue(tpe: c.Type): FullState[c.Tree] = {
      State.get[VState].map(_._2).flatMap { values =>
        values.get(tpe) match {
          case Some(value) => State.pure(value)
          case None =>
            for {
              _ <- createIfNotExists(tpe)
              value <- buildValue(tpe)
              _ <- updateIfNotExists(tpe, value)
            } yield value
        }
      }
    }

    def initValues(tree: Option[c.Tree]): FullState[Unit] = {
      tree.fold {
        getOrElseCreateValue(typeToGen).as(())
      } { value =>
        for {
          _ <- updateIfNotExists(typeToGen, c.untypecheck(value.duplicate))
          _ <- createIfNotExists(typeToGen)
          variables <- State.get[VState].map(_._1)
          _ <- (variables.keySet - typeToGen).traverse { tpe =>
            findImplicit(tpe).flatMap(updateIfNotExists(tpe, _))
          }
        } yield ()
      }
    }

    type Methods = List[Method]
    type Selector = List[Optic]

    sealed trait Optic
    case class Lens(fromType: c.Type, field: c.TermName, toType: c.Type) extends Optic
    case class Prism(toType: c.Type) extends Optic

    def focusWithPrism(tpe: c.Type, toType: c.Type)(next: VarsState[CustomRepr]): VarsState[CustomRepr] = {
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
            State.getOrElseUpdate(subtype, c.freshName(subtype.typeSymbol.name).toTermName)
              .map { name =>
                subtype -> NotSpecified(name)
              }
          }
        }.map(_.toMap).map(SpecifiedSealedTrait)
      }
    }

    def focusWithLens(tpe: c.Type, fromType: c.Type, field: c.TermName)(next: c.Symbol => VarsState[CustomRepr]): VarsState[CustomRepr] = {
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
            State.getOrElseUpdate(paramType, c.freshName(paramType.typeSymbol.name).toTermName).map { name =>
              param.name.toTermName -> NotSpecified(name)
            }
          }
        }.map(_.toMap)
      }.map(SpecifiedCaseClass(tpe, _))
    }

    sealed trait Method {
      def toSpecifiedGen(tpe: c.Type): VarsState[CustomRepr]
    }

    case class UseDefaultMethod(selector: Selector, pos: c.Position) extends Method {
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

    case class ExcludeMethod(selector: Selector, pos: c.Position) extends Method {
      override def toSpecifiedGen(tpe: c.Type): VarsState[CustomRepr] = toSpecifiedGen(tpe, selector)

      private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[CustomRepr] = optics match {
        case Nil => State.pure(Excluded(pos))
        case Lens(_, field, _) :: Nil => fail(s"Path in .exclude(...) must end with .when[...], not with argument $field")
        case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
        case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
      }
    }

    case class SpecifyMethod(selector: Selector, arg: Arg, pos: c.Position) extends Method {
      override def toSpecifiedGen(tpe: c.Type): VarsState[CustomRepr] = toSpecifiedGen(tpe, selector)

      private def toSpecifiedGen(tpe: c.Type, optics: List[Optic]): VarsState[CustomRepr] = optics match {
        case Prism(toType) :: tail => focusWithPrism(tpe, toType)(toSpecifiedGen(toType, tail))
        case Lens(fromType, field, toType) :: tail => focusWithLens(tpe, fromType, field)(_ => toSpecifiedGen(toType, tail))
        case Nil => State.pure(Specified(arg, pos))
      }
    }

    sealed trait Arg
    case class GenArg(tree: c.Tree) extends Arg
    case class ConstArg(tree: c.Tree) extends Arg
    case class DefaultArg(tree: c.Tree) extends Arg

    @tailrec
    def disassembleTree(tree: c.Tree, methods: Methods = List.empty): Methods = {
      def mkPos(other: c.Tree): c.Position = tree.pos.withStart(other.pos.end + 1)

      tree match {
        case q"$other.specify[$_](($_) => $selectorTree)($arg)" =>
          val selector = disassembleSelector(selectorTree)
          val method = SpecifyMethod(selector, GenArg(arg), mkPos(other))
          disassembleTree(other, method +: methods)
        case q"$other.specifyConst[$_](($_) => $selectorTree)($arg)" =>
          val selector = disassembleSelector(selectorTree)
          val method = SpecifyMethod(selector, ConstArg(arg), mkPos(other))
          disassembleTree(other, method +: methods)
        case q"$other.useDefault[$_](($_) => $selectorTree)" =>
          val selector = disassembleSelector(selectorTree)
          val method = UseDefaultMethod(selector, mkPos(other))
          disassembleTree(other, method +: methods)
        case q"$other.exclude[$_](($_) => $selectorTree)" =>
          val selector = disassembleSelector(selectorTree)
          val method = ExcludeMethod(selector, mkPos(other))
          disassembleTree(other, method +: methods)
        case q"$module.custom[$_]" if module.symbol == genSymbol => methods
        case _ => c.abort(tree.pos, "Unsupported syntax")
      }
    }

    @tailrec
    def disassembleSelector(tree: c.Tree, selector: Selector = List.empty): Selector = {
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
          if (from.symbol.isAbstract && !from.symbol.asClass.isSealed) c.abort(to.pos, s"Type $from is not sealed")
          val prism = Prism(to.tpe)
          disassembleSelector(other, prism :: selector)
        case q"$_[$from]($other).when[$to](..$_)" =>
          if (from.symbol.isAbstract && !from.symbol.asClass.isSealed) c.abort(to.pos, s"Type $from is not sealed")
          val toType = subclassType(to.symbol, from.tpe)
          val prism = Prism(toType)
          disassembleSelector(other, prism :: selector)
        case _: Ident => selector
        case tree => c.abort(tree.pos, "Unsupported path element")
      }
    }

    trait HasPosition {
      def pos: c.Position
    }

    sealed trait CustomRepr

    sealed trait OpticRepr extends CustomRepr
    case class SpecifiedCaseClass(tpe: c.Type, fields: List[Map[c.TermName, CustomRepr]]) extends OpticRepr
    case class SpecifiedSealedTrait(subclasses: Map[c.Type, CustomRepr]) extends OpticRepr

    sealed trait SpecifiedRepr extends CustomRepr with HasPosition
    case class Specified(tree: Arg, pos: c.Position) extends SpecifiedRepr
    object Specified {
      def unapply(s: Specified): Option[Arg] = Some(s.tree)
    }
    case class Excluded(pos: c.Position) extends SpecifiedRepr

    sealed trait NotSpecifiedRepr extends CustomRepr
    case class NotSpecifiedImplicit(tree: c.Tree) extends NotSpecifiedRepr
    case class NotSpecified(name: c.TermName) extends NotSpecifiedRepr

    def usedVariables(gen: CustomRepr): Set[c.TermName] = gen match {
      case SpecifiedCaseClass(_, fields) => fields.flatten.flatMap { case (name, field) =>
        usedVariables(field) + name
      }.toSet
      case SpecifiedSealedTrait(subclasses) => subclasses.values.toSet.flatMap(usedVariables)
      case NotSpecified(tree) => Set(tree)
      case _ => Set.empty
    }

    def deleteUnused(gen: CustomRepr, variables: Variables): Variables = {
      val tpeA = typeToGen
      val used = usedVariables(gen)
      variables.filter { case (tpe, name) =>
        used.contains(name) || tpe == tpeA
      }
    }

    def mergeMethods(methods: Methods): VarsState[Option[CustomRepr]] = {
      methods.traverse(_.toSpecifiedGen(typeToGen)).map { list =>
        list.reduceOption {
          mergeSpecifications(_, _)
            .getOrElse(fail(aggregateErrors(list)))
        }
      }
    }

    def showPos(pos: c.Position): String = {
      val p = Position.range(pos.source, pos.start, pos.point, pos.end)
      p.source.lines(
        p.source.offsetToLine(p.start),
        p.source.offsetToLine(p.end) + 1
      ).mkString("\n")
    }

    case class Conflict(pos: c.Position, withPos: c.Position) {
      override def toString: String =
        s"""${showPos(pos)}
           |and
           |${showPos(withPos)}
           |""".stripMargin
    }

    def aggregateErrors(list: List[CustomRepr]): String = {
      aggregate(list).zipWithIndex.map { case (conf, ind) =>
        s"""
           |${ind + 1}.
           |$conf""".stripMargin
      }.mkString("Conflicts:", "", "")
    }

    def aggregate(list: List[CustomRepr]): List[Conflict] = {
      list.foldLeft((Set.empty[CustomRepr], List.empty[Conflict])) { case ((prev, acc), repr) =>
        val res = prev.map(mergeSpecifications(_, repr)).collect {
          case Left(value) => value
        }
        (prev + repr, acc ++ res)
      }._2
    }

    def mergeSpecifications(
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

    def getPositions(customRepr: CustomRepr): List[c.Position] = {
      customRepr match {
        case SpecifiedCaseClass(_, fields) => fields.flatMap(_.values.flatMap(getPositions))
        case SpecifiedSealedTrait(subclasses) => subclasses.values.flatMap(getPositions).toList
        case repr: SpecifiedRepr => List(repr.pos)
        case _: NotSpecifiedRepr => List()
      }
    }

    def specifiedTree(termName: c.TermName)(gen: CustomRepr): c.Tree = gen match {
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

    def isAbstractSealed(sym: c.Symbol): Boolean = {
      sym.isAbstract && sym.isClass && sym.asClass.isSealed
    }

    def isConcreteClass(sym: c.Symbol): Boolean = {
      sym.isClass && !sym.isAbstract
    }

    def toGen(termName: c.TermName)(tree: c.Tree): c.Tree = {
      val f: c.Tree = Function(ValDef(Modifiers(Flag.PARAM), termName, TypeTree(), EmptyTree) :: Nil, tree)
      q"_root_.dev.aliakovl.gin.Gen.apply($f)"
    }

    def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Gen.const($tree)"

    def callApply(tree: c.Tree)(termName: c.TermName): c.Tree = q"$tree.apply(${Ident(termName)})"

    def constValueOf(tpe: c.Type): c.Tree = toConst(q"_root_.scala.Predef.valueOf[$tpe]")

    def constructType[F[_]](tpe: c.Type)(implicit weakTypeTag: WeakTypeTag[F[_]]): c.Type = {
      appliedType(weakTypeTag.tpe.typeConstructor, tpe)
    }

    def construct(tpe: c.Type, constructorArgs: List[List[c.Tree]]): c.Tree = {
      val constructionMethodTree: Tree = Select(New(Ident(tpe.dealias.typeSymbol)), termNames.CONSTRUCTOR)
      constructorArgs.foldLeft(constructionMethodTree)(Apply.apply)
    }

    def constructCases[T](termName: c.TermName)(subclasses: Map[c.Type, T])(toTree: T => c.Tree): c.Tree = {
      val cases = subclasses.toList.sortBy(_._1.typeSymbol.fullName).map(_._2)
      cases match {
        case singleCase :: Nil => toTree(singleCase)
        case _ => q"$termName.nextInt(${cases.size}) match { case ..${
          cases.zipWithIndex.map {
            case value -> index => cq"$index => ${toTree(value)}"
          }
        } }"
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
          fail(s"Child $child of $parent is neither final nor a case class")
        }
      }

      concreteChildren ++ abstractChildren.flatMap { child =>
        val childClass = child.asClass
        if (childClass.isSealed) {
          subclassesOf(childClass)
        } else {
          fail(s"Child $child of $parent is not sealed")
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
        .getOrElse(fail(s"Class ${tpe.typeSymbol.name} has no public constructors"))
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
          trait PatchedLookupResult {
            def suchThat(criterion: Symbol => Boolean): Symbol
          }

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

    def make(prefix: c.Tree): FullState[Unit] = {
      Option.when(typeToGen.typeSymbol.isClass) {
        mergeMethods(disassembleTree(prefix))
      }
      .sequence
      .map(_.flatten)
      .flatTap { genOpt =>
        genOpt.traverse { gen =>
          State.modify[Variables](deleteUnused(gen, _))
        }
      }
      .map { genOpt =>
        genOpt.map { gen =>
          withName { termName =>
            toGen(termName)(specifiedTree(termName)(gen))
          }
        }
      }
      .modifyState[VState].flatMap(initValues)
    }

    def materialize(): FullState[Unit] = initValues(None)

    withStateProvided {
      c.macroApplication match {
        case q"$_.materialize[$_]" => materialize()
        case q"$prefix.make" => make(prefix)
      }
    }((genTree _).tupled)
  }

  object Lazy {
    def apply[T](variable: String): T = ???
  }
}
