import dev.aliakovl.reflex.Macros
import dev.aliakovl.reflex.Macros.test

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

case class Person(name: String)

case class Purchase(name: String, orderNumber: Int, var shipped: Boolean)

class Animal
class Cat extends Animal

object Main {
  val l = List(1, 2, 3)

  def getTypeTag[T: ru.TypeTag](obj: T): ru.TypeTag[T] = ru.typeTag[T]
  def getType[T: ru.TypeTag](obj: T) = typeOf[T]

  val theType = getTypeTag(l).tpe
  val decls = theType.decls.take(10)

  val m: ru.Mirror = ru.runtimeMirror(getClass.getClassLoader)
  val classPerson: ru.ClassSymbol = ru.typeOf[Person].typeSymbol.asClass
  val cm: ru.ClassMirror = m.reflectClass(classPerson)
  val ctor: ru.MethodSymbol =
    ru.typeOf[Person].decl(ru.termNames.CONSTRUCTOR).asMethod
  val ctorm: ru.MethodMirror = cm.reflectConstructor(ctor)
  val mike = ctorm("Mike")

  val p = Purchase("Jeff Lebowski", 23819, false)
  val shippingTermSymb: ru.TermSymbol =
    ru.typeOf[Purchase].decl(ru.TermName("shipped")).asTerm
  val im: ru.InstanceMirror = m.reflect(p)
  val shippingFieldMirror: ru.FieldMirror = im.reflectField(shippingTermSymb)

  def printWeakType[A](obj: List[A]): Unit = {
    println(weakTypeOf[List[A]])
  }

  val tree = Apply(
    Select(
      Apply(
        Select(Ident(TermName("x")), TermName("$plus")),
        List(Literal(Constant(2)))
      ),
      TermName("$plus")
    ),
    List(Literal(Constant(3)))
  )

  def main(args: Array[String]): Unit = {
    println(decls)
    println(mike)

    println(shippingFieldMirror.get)
    shippingFieldMirror.set(true)
    println(shippingFieldMirror.get)

    println(p)

    println(Macros.currentLocation)

    println(getType(List(1, 2, 3)))
    println(getType(new Animal))
    println(getType(new Cat))

    printWeakType(List(3, 4, 5))

    ru.definitions.LongTpe

    println(typeOf[Cat] <:< typeOf[Animal])
    println(typeOf[Cat] =:= typeOf[Cat])

    typeOf[List[Int]].members.filter(_.isConstructor).foreach(println)

    println(show(tree))

    val expr = reify { class Flower { def name = "Rose" } }

    println(show(expr.tree))
    println(showRaw(expr.tree))

    val Apply(fun, arg :: Nil) = tree
    println(fun)
    println(arg)

    object traverser extends Traverser {
      var applies = List[Apply]()

      override def traverse(tree: Tree): Unit = tree match {
        case app @ Apply(fun, args) =>
          applies = app :: applies
          super.traverse(fun)
          super.traverseTrees(args)
        case _ => super.traverse(tree)
      }

    }

    traverser.traverse(tree)
    println(traverser.applies)

    { val tree = reify(println(2)).tree; println(showRaw(tree)) }

    {
      val x = reify(2)

      println(showRaw(reify(println(x.splice)).tree))
    }

    {
      val fn = reify(println)

      println(showRaw(reify(fn.splice).tree))
    }


    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
      println(showRaw(tb.parse("println(2)")))


    test

    {
      val t = Apply(Ident(TermName("println")), List(Literal(Constant(2))))
      tb.eval(t)
    }



  }
}
