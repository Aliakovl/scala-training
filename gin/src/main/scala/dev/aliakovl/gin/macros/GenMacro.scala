package dev.aliakovl.gin
package macros

import dev.aliakovl.gin.macros.fp.data.State
import dev.aliakovl.gin.macros.fp.syntax._

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

final class GenMacro(val c: whitebox.Context) extends Common {
  import c.universe._

  def makeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = Stack.withContext[Gen[A]](c) { stack =>
    import stack._

    val typeToGen = weakTypeOf[A].dealias
    val genSymbol = symbolOf[Gen.type].asClass.module
    val ginModule = typeOf[dev.aliakovl.gin.`package`.type].termSymbol.asModule

    def genTree(vState: VState): c.Expr[Gen[A]] = {
      val variables = vState.variables
      val values = vState.values

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
        State.pure(toConst(toValueOf(tpe)))
      } else if (sym.isAbstract && sym.isClass && !sym.asClass.isSealed) {
        c.abort(c.enclosingPosition, s"Can not build Gen[$tpe], because abstract type $tpe is not sealed. Try to provide it implicitly")
      } else if (isAbstractSealed(sym)) {
        subTypesOf(tpe).traverse { subtype =>
          getVariableName(subtype).map(subtype -> _)
        }.map { subclasses =>
          if (subclasses.isEmpty) fail(s"Class ${tpe.typeSymbol.name} does not have constructors")
          withName { termName =>
            toGen(termName)(constructCases(termName)(subclasses.toMap) { name => callApply(Ident(name))(termName) })
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
        .map(tree => c.untypecheck(pullOutLazyVariables.transform(tree))) <* createVariableIfNotExists(tpe)
    }

    def createValueIfNotExists(tpe: c.Type, value: c.Tree): FullState[Unit] = {
      State.modifyUnless[VState](_.values.contains(tpe))(_.modify[Values](_.updated(tpe, value)))
    }

    def createVariableIfNotExists(tpe: c.Type): FullState[Unit] = {
      State.modifyUnless[VState](_.variables.contains(tpe))(_.modify[Variables]{ variables =>
        val name = c.freshName(tpe.typeSymbol.name).toTermName
        variables.updated(tpe, name)
      })
    }

    def getVariableName(tpe: c.Type): FullState[TermName] = for {
      variables <- State.get[VState].map(_.variables)
      name <- State.pure(variables.get(tpe)).fallback {
        findImplicit(tpe).flatMap(createValueIfNotExists(tpe, _)) *> State.get[VState].map(_.variables(tpe))
      }
    } yield name

    def getOrElseCreateValue(tpe: c.Type): FullState[c.Tree] = {
      State.get[VState].map(_.values).flatMap { values =>
        values.get(tpe) match {
          case Some(value) => State.pure(value)
          case None =>
            for {
              _ <- createVariableIfNotExists(tpe)
              value <- buildValue(tpe)
              _ <- createValueIfNotExists(tpe, value)
            } yield value
        }
      }
    }

    def createDependencies(value: c.Tree): FullState[Unit] = {
      for {
        _ <- createValueIfNotExists(typeToGen, c.untypecheck(value.duplicate))
        _ <- createVariableIfNotExists(typeToGen)
        variables <- State.get[VState].map(_.variables)
        _ <- (variables.keySet - typeToGen).traverse { tpe =>
          findImplicit(tpe).flatMap(createValueIfNotExists(tpe, _))
        }
      } yield ()
    }

    def initValues(tree: Option[c.Tree]): FullState[Unit] = {
      tree.fold {
        getOrElseCreateValue(typeToGen).unit
      }(createDependencies)
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
      def mkPos(other: c.Tree): c.Position = tree.pos.withStart(other.pos.end)

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
      val used = usedVariables(gen)
      variables.filter { case (tpe, name) =>
        used.contains(name) || tpe == typeToGen
      }
    }

    def mergeMethods(methods: Methods): VarsState[Option[CustomRepr]] = {
      methods.traverse(_.toSpecifiedGen(typeToGen)).map { representations =>
        representations.reduceOption {
          mergeSpecifications(_, _)
            .getOrElse(fail(printConflicts(aggregateConflicts(representations))))
        }
      }
    }

    case class Conflict(pos: c.Position, withPos: c.Position) {
      def asString(i: Int): String = {
        s"""$i. ${pos.source.path}:${pos.line}
           |${showPos(pos)}
           |with ${withPos.source.path}:${withPos.line}
           |${showPos(withPos)}
           |""".stripMargin
      }
    }

    def printConflicts(conflicts: List[Conflict]): String = {
      conflicts.zipWithIndex.map { case (conflict, ind) =>
        conflict.asString(ind + 1)
      }.mkString("Conflicts:\n", "", "")
    }

    def aggregateConflicts(representations: List[CustomRepr]): List[Conflict] = {
      representations.foldLeft((Set.empty[CustomRepr], List.empty[Conflict])) { case ((prevs, acc), repr) =>
        val res = prevs.map(mergeSpecifications(_, repr)).collect {
          case Left(value) => value
        }
        (prevs + repr, acc ++ res)
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

    def toGen(termName: c.TermName)(tree: c.Tree): c.Tree = {
      val f: c.Tree = Function(ValDef(Modifiers(Flag.PARAM), termName, TypeTree(), EmptyTree) :: Nil, tree)
      q"_root_.dev.aliakovl.gin.Gen.apply($f)"
    }

    def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Gen.const($tree)"

    def toValueOf(tpe: c.Type): c.Tree = q"_root_.scala.Predef.valueOf[$tpe]"

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

    def make(prefix: c.Tree): FullState[Unit] = {
      mergeMethods(disassembleTree(prefix))
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
      .modifyState[VState]
      .flatMap(initValues)
    }

    def materialize(): FullState[Unit] = getOrElseCreateValue(typeToGen).unit

    def checkType(tpe: c.Type): Unit = {
      tpe match {
        case RefinedType(_, _) => fail(s"Could not infer Gen for refined type $tpe")
        case ExistentialType(_, _) => fail(s"Could not infer Gen for existential type $tpe")
        case t if !t.typeSymbol.isClass => fail(s"Could not infer Gen for non class type $tpe")
        case _ => ()
      }
    }

    checkType(typeToGen)

    withStateProvided {
      c.macroApplication match {
        case q"$_.materialize[$_]" => materialize()
        case q"$prefix.make" => make(prefix)
      }
    }(genTree)
  }
}
