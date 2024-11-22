package dev.aliakovl.gin
package macros

import dev.aliakovl.gin.macros.fp.data.State
import dev.aliakovl.gin.macros.fp.syntax._

import scala.reflect.macros.whitebox

final class GenMacro(val c: whitebox.Context) extends Common with MacroState with MacroCustom {
  import c.universe._

  def makeImpl[A: c.WeakTypeTag]: c.Expr[Gen[A]] = Stack.withContext[VState, Gen[A]](c) { stack =>
    import stack._

    val typeToGen = weakTypeOf[A].dealias

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

    def toGen(termName: c.TermName)(tree: c.Tree): c.Tree = {
      val f: c.Tree = Function(ValDef(Modifiers(Flag.PARAM), termName, TypeTree(), EmptyTree) :: Nil, tree)
      q"_root_.dev.aliakovl.gin.Gen.apply($f)"
    }

    def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Gen.const($tree)"

    def toValueOf(tpe: c.Type): c.Tree = q"_root_.scala.Predef.valueOf[$tpe]"

    def make(prefix: c.Tree): FullState[Unit] = {
      mergeMethods(disassembleTree(prefix), typeToGen)
      .flatTap { genOpt =>
        genOpt.traverse { gen =>
          State.modify[Variables](deleteUnused(gen, _, typeToGen))
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
