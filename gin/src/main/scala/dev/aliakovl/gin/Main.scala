package dev.aliakovl.gin

import dev.aliakovl.gin.GenRule.URI
import dev.aliakovl.gin.Mapped.MappedOps
import shapeless.labelled.{FieldType, field}
import shapeless._
import shapeless.ops.hlist.Mapper

sealed trait Sex
case object Male extends Sex
case object Female extends Sex

case class User(age: Int, name: String, sex: Option[Sex])

trait Generator[A] {
  type Func[T] <: () => T
  def apply: Func[A]
}

object Generator {
  def apply[A](implicit rule: Generator[A]): Generator[A] = rule

  type Aux[A, F[T] <: () => T] = Generator[A] { type Func[T] = F[T] }

  def const[A](value: => A): Generator.Aux[A, Function0] = new Generator[A] {
    override type Func[T] = () => T
    override def apply: () => A = () => value
  }

  def random[A](implicit random: Random[A]): Generator.Aux[A, Random] =
    new Generator[A] {
      override type Func[T] = Random[T]
      override def apply: Random[A] = random
    }

  implicit val intGenerator: Generator[Int] = Generator.random[Int]

  implicit val stringRandom: Generator[String] = Generator.random[String]

  implicit def genericGenerator[A, R](implicit
      generic: LabelledGeneric.Aux[A, R],
      gen: Lazy[Generator[R]]
  ): Generator[A] = new Generator[A] {
    override type Func[T] = () => T
    override def apply: () => A = () => generic.from(gen.value.apply())
  }

  implicit val hnilGenerator: Generator[HNil] = Generator.const(HNil)

  implicit def hlistGenerator[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hGenerator: Lazy[Generator[H]],
      tGenerator: Generator[T]
  ): Generator[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    println(fieldName)
    Generator.const(
      field[K](hGenerator.value.apply()) :: tGenerator.apply()
    )
  }

  implicit val cnilGenerator: Generator[CNil] = const(
    throw new Exception("Unreachable")
  )

  implicit def coproductGenerator[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      hGenerator: Lazy[Generator[H]],
      tGenerator: Generator[T]
  ): Generator[FieldType[K, H] :+: T] = {
    val fieldName = witness.value.name
    println(fieldName)
    Generator.const[FieldType[K, H] :+: T](
      Inl[FieldType[K, H], T](field[K](hGenerator.value.apply()))
    )
  }

  object convertion extends Poly1 {
    implicit def toGenerator[A]: Case.Aux[A, Generator[A]] =
      at(a => Generator.const(a))
  }

  def mapped[A](a: A)(implicit m: Mapped[A, convertion.type]): m.Out_ = {
    m(a)
  }
}

trait Mapped[A, P] {
  type Out_
  def apply(a: A): Out_
}

object Mapped {
  type Aux[A, P, O] = Mapped[A, P] { type Out_ = O }

  def createMapped[A, P, O](f: A => O): Aux[A, P, O] = new Mapped[A, P] {
    override type Out_ = O
    override def apply(a: A): O = f(a)
  }

  implicit def mapProduct[A, P <: Poly, In <: HList, Out <: HList](implicit
      generic: Generic.Aux[A, In],
      mapper: Mapper.Aux[P, In, Out]
  ): Aux[A, P, Out] = createMapped(a => mapper(generic.to(a)))

  implicit def mapHList[In <: HList, P <: Poly, Out <: HList](implicit
      mapper: Mapper.Aux[P, In, Out]
  ): Aux[In, P, Out] = createMapped(a => mapper(a))

  implicit def toProduct[B, In <: HList, P <: Poly, Out <: HList](implicit
      generic: Generic.Aux[B, Out],
      mapper: Mapper.Aux[P, In, Out]
  ): Aux[In, P, B] = createMapped(a => generic.from(mapper(a)))

  implicit class MappedOps[A](a: A) {
    class Builder[Out] {
      def apply[P <: Poly](poly: P)(implicit m: Aux[A, P, Out]): Out =
        m.apply(a)
    }

    def map[Out]: Builder[Out] = new Builder[Out]
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(Generator.const(4).apply())
    println(Generator.apply[Sex].apply())

    val r: Generator[Int] :: Generator[String] :: HNil = Generator.mapped((3, "wefwef"))

    println(r)

    val a = Generator.random[URI].apply()

    println(GenRule.gen(a))

  }
}
