package dev.aliakovl.reflex

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Macros {
  def currentLocation: Location = macro impl

  def assert(cond: Boolean, msg: Any): Unit = macro assertImpl

  def impl(c: whitebox.Context): c.Expr[Location] = {
    import c.universe._

    val pos: c.universe.Position = c.macroApplication.pos
    val clsLocation = c.mirror.typeOf[Location.type].termSymbol
    c.Expr(Apply(Ident(clsLocation), List(Literal(Constant(pos.source.path)), Literal(Constant(pos.line)), Literal(Constant(pos.column)))))
  }



  def test: Unit = macro printImpl

  def printImpl(c: whitebox.Context): c.Expr[Unit] = c.Expr(c.parse("println(2)"))

  def assertImpl(c: whitebox.Context)(cond: c.Expr[Boolean], msg: c.Expr[Any]): c.Expr[Unit] = {
    ???
  }

  def printf(format: String, params: Any*): Unit = macro printf_impl

  def printf_impl(c: whitebox.Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val Literal(Constant(s_format: String)) = format.tree

    val evals = ListBuffer[ValDef]()

    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = TermName(c.freshName("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val paramsStack = mutable.Stack[Tree]((params map (_.tree)): _*)
    val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => precompute(paramsStack.pop, typeOf[Int])
      case "%s" => precompute(paramsStack.pop, typeOf[String])
      case "%%" => Literal(Constant("%"))
      case part => Literal(Constant(part))
    }

    val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
  }

}
