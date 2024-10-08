package dev.aliakovl.shapelessguide.examples

import shapeless._
import shapeless.ops.coproduct.Length
import shapeless.ops.nat.ToInt

trait Random[A] {
  def get: A
}

object Random {
  def random[A](implicit r: Random[A]): A = r.get

  def createRandom[A](func: () => A): Random[A] =
    new Random[A] {
      def get: A = func()
    }

  implicit val intRandom: Random[Int] =
    createRandom(() => scala.util.Random.nextInt(10))

  implicit val charRandom: Random[Char] =
    createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)

  implicit val booleanRandom: Random[Boolean] =
    createRandom(scala.util.Random.nextBoolean)

  implicit def genericRandom[A, R](implicit
      gen: Generic.Aux[A, R],
      random: Lazy[Random[R]]
  ): Random[A] = createRandom(() => gen.from(random.value.get))

  implicit val hnilRandom: Random[HNil] =
    createRandom(() => HNil)

  implicit def hlistRandom[H, T <: HList](implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T]
  ): Random[H :: T] =
    createRandom(() => hRandom.value.get :: tRandom.get)

  implicit val cnimRandom: Random[CNil] =
    createRandom(() => throw new Exception("Inconceivable!"))

  implicit def coproductRandom[H, T <: Coproduct, L <: Nat](implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T],
      tLength: Length.Aux[T, L],
      tLengthAsInt: ToInt[L]
  ): Random[H :+: T] = createRandom { () =>
    val length = 1 + tLengthAsInt()
    val chooseH = scala.util.Random.nextDouble() < (1.0 / length)
    if (chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
  }

}
