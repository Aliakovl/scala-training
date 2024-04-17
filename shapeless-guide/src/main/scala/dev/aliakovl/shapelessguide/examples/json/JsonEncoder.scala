package dev.aliakovl.shapelessguide.examples.json

import dev.aliakovl.shapelessguide.examples._
import dev.aliakovl.shapelessguide.examples.json.JsonObjectEncoder.createObjectEncoder
import shapeless._
import shapeless.labelled.FieldType

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): JsonValue = func(value)
    }

  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => JsonString(str))

  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(num => JsonNumber(num))

  implicit val intEncoder: JsonEncoder[Int] =
    createEncoder(num => JsonNumber(num))

  implicit val booleanEncoder: JsonEncoder[Boolean] =
    createEncoder(bool => JsonBoolean(bool))

  implicit def listEncoder[A](implicit
      enc: JsonEncoder[A]
  ): JsonEncoder[List[A]] =
    createEncoder(list => JsonArray(list.map(enc.encode)))

  implicit def optionEncoder[A](implicit
      enc: JsonEncoder[A]
  ): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))

  implicit def genericObjectEncoder[A, H](implicit
      generic: LabelledGeneric.Aux[A, H],
      hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

object JsonObjectEncoder {
  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject =
        fn(value)
    }

  implicit val hnilObjectEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[JsonEncoder[H]],
      tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    createObjectEncoder { case h :: t =>
      JsonObject(
        (fieldName -> hEncoder.value.encode(h)) :: tEncoder.encode(t).fields
      )
    }
  }

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
    createObjectEncoder(_ => throw new Exception("Inconceivable!"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[JsonEncoder[H]],
      tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val fieldName = witness.value.name
    createObjectEncoder {
      case Inl(h) => JsonObject(List(fieldName -> hEncoder.value.encode(h)))
      case Inr(t) => tEncoder.encode(t)
    }
  }
}
