package dev.aliakovl.gin.macros

import java.lang.System.lineSeparator
import scala.reflect.internal.util.Position
import scala.reflect.macros.whitebox

private[macros] trait CommonMacros {
  val c: whitebox.Context

  import c.universe._

  def fail(message: String): Nothing = c.abort(c.enclosingPosition, message)

  def showPos(pos: c.Position): String = {
    val sourceCode = pos.source.sourceAt {
      Position.range(pos.source, pos.start, pos.point, pos.end)
    }
    sourceCode
      .stripLeading()
      .stripTrailing()
      .linesIterator
      .map(Console.BLACK_B + Console.RED + _ + Console.RESET)
      .mkString(lineSeparator)
  }

  def withName[T](body: c.TermName => T): T = body(TermName(c.freshName()))

  def block[T](statements: List[c.Expr[Any]], expr: c.Expr[T]): c.Expr[T] =
    c.Expr[T](q"..$statements; $expr")

  object Lazy {
    def apply[T](variable: String): T = ???
  }

  object LazyRef {
    private val symbol = symbolOf[Lazy.type].asClass.module

    def apply(tpe: c.Type, variable: String): c.Tree =
      q"$symbol.apply[$tpe]($variable)"

    def unapply(tree: c.Tree): Option[String] = tree match {
      case q"$module.apply[$_](${Literal(Constant(variable: String))})" if module.symbol == symbol => Some(variable)
      case _ => None
    }
  }

  val pullOutLazyVariables: Transformer = new Transformer {
    override def transform(tre: c.Tree): c.Tree = tre match {
      case LazyRef(variable) => Ident(TermName(variable))
      case _                   => super.transform(tre)
    }
  }

  def isAbstractSealed(sym: c.Symbol): Boolean = {
    sym.isAbstract && sym.isClass && sym.asClass.isSealed
  }

  def isConcreteClass(sym: c.Symbol): Boolean = {
    sym.isClass && !sym.isAbstract
  }

  def callApply(tree: c.Tree)(termName: c.TermName): c.Tree = q"$tree.apply(${Ident(termName)})"

  def constructType[F[_]](tpe: c.Type)(implicit weakTypeTag: WeakTypeTag[F[_]]): c.Type = {
    appliedType(weakTypeTag.tpe.typeConstructor, tpe)
  }

  def construct(tpe: c.Type, constructorArgs: List[List[c.Tree]]): c.Tree = {
    val constructionMethodTree: Tree = Select(New(Ident(tpe.dealias.typeSymbol)), termNames.CONSTRUCTOR)
    constructorArgs.foldLeft(constructionMethodTree)(Apply.apply)
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

  def subTypesOf(parent: c.Type): Set[c.Type] =
    subclassesOf(parent.typeSymbol.asClass).map(subclassType(_, parent))

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

  def toValueOf(tpe: c.Type): c.Tree = q"_root_.scala.Predef.valueOf[$tpe]"

  def toGen(termName: c.TermName)(tree: c.Tree): c.Tree = {
    val f: c.Tree = Function(ValDef(Modifiers(Flag.PARAM), termName, TypeTree(), EmptyTree) :: Nil, tree)
    q"_root_.dev.aliakovl.gin.Gen.apply($f)"
  }

  def toConst(tree: c.Tree): c.Tree = q"_root_.dev.aliakovl.gin.Gen.const($tree)"

  def lazyVal(variable: c.TermName, tpe: c.Type, value: c.Tree): c.Expr[Any] = c.Expr[Any] {
    q"lazy val $variable: _root_.dev.aliakovl.gin.Gen[$tpe] = $value"
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

  private def subclassesOf(parent: ClassSymbol): Set[c.Symbol] = {
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

  private def isPhantomConstructor(constructor: c.Symbol): Boolean = constructor.asMethod.fullName.endsWith("$init$")
}
