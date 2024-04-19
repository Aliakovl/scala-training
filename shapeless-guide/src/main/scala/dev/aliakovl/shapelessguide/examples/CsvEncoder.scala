package dev.aliakovl.shapelessguide.examples

import shapeless._

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] = func(value)
    }

  implicit val stringEncoder: CsvEncoder[String] =
    createEncoder(str => List(str))

  implicit val intEncoder: CsvEncoder[Int] =
    createEncoder(num => List(num.toString))

  implicit val doubleEncoder: CsvEncoder[Double] =
    createEncoder(num => List(num.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    createEncoder(bool => List(if (bool) "yes" else "no"))

  implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(_ => Nil)

  implicit def hlistEncoder[H, T <: HList](implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] = createEncoder { case h :: t =>
    hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](implicit
      gen: Generic.Aux[A, R],
      rEncoder: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] = createEncoder(a => rEncoder.value.encode(gen.to(a)))

  implicit def cnilEncoder: CsvEncoder[CNil] =
    createEncoder(_ => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(head) => hEncoder.value.encode(head)
    case Inr(tail) => tEncoder.encode(tail)
  }
}
