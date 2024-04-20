package dev.aliakovl.gin

import cats.Functor
import cats.syntax.all._

sealed trait Sex
case object Male extends Sex
case object Female extends Sex

case class User(age: Int, name: String, sex: Option[Sex])

// обобщение: поведение зависит от класса типов превала генерации GenRule[F[_], A] = GenRule(fa: F[A])
sealed trait GenRule[A]
case class ConstRule[A](value: A) extends GenRule[A]
case class RandomRule[A](random: Random[A]) extends GenRule[A]

object GenRule {
  implicit val genRuleFunctor: Functor[GenRule] = new Functor[GenRule] {
    override def map[A, B](fa: GenRule[A])(f: A => B): GenRule[B] = {
      fa match {
        case ConstRule(value)   => ConstRule(f(value))
        case RandomRule(random) => RandomRule(random.map(f))
      }
    }
  }
}

trait Generator[A] {
  def generate(rule: GenRule[A]): A

  def random(implicit random: Random[A]): A = generate(RandomRule(random))
  def custom(a: A): A = generate(ConstRule(a))
}

object Generator {
  def apply[A](implicit gen: Generator[A]): Generator[A] = gen

  def create[A](f: GenRule[A] => A): Generator[A] = rule => f(rule)

  implicit def generatorA[A]: Generator[A] = create {
    case ConstRule(value)   => value
    case RandomRule(random) => random.get
  }

//  implicit def genericGenerator[A, R](implicit
//      gen: Generic.Aux[A, R],
//      generator: Lazy[Generator[R]]
//  ): Generator[A] = create[A] { rule =>
//    gen.from(generator.value.generate(rule.map(gen.to)))
//  }
//
//  implicit val hnilGenerator: Generator[HNil] = create(_ => HNil)
//
//  implicit def hlistGenerator[H, T <: HList](implicit
//      hGenerator: Lazy[Generator[H]],
//      tGenerator: Generator[T]
//  ): Generator[H :: T] = create[H :: T] {
//    case ConstRule(value) => value
//    case RandomRule(random) =>
//      hGenerator.value.random :: tGenerator.random
//  }
//
//  implicit val cnilGenerator: Generator[CNil] = create[CNil] { _ =>
//    throw new RuntimeException("Unreachable")
//  }
//
//  implicit def coproductGenarator[H, T <: Coproduct, L <: Nat](implicit
//      hGenerator: Lazy[Generator[H]],
//      tGenerator: Generator[T],
//      tLength: Length.Aux[T, L],
//      tLengthAsInt: ToInt[L]
//  ): Generator[H :+: T] = create {
//    case ConstRule(value) => value
//    case RandomRule() =>
//      val length = 1 + tLengthAsInt()
//      val chooseH = scala.util.Random.nextDouble < (1.0 / length)
//      if (chooseH) Inl(hGenerator.value.random)
//      else Inr(tGenerator.random)
//  }
}

object Main {
//  implicit val genInt: Generator[Int] = Generator.create {
//    case ConstRule(value) => value
//    case RandomRule()     => Random.nextInt()
//  }
//
//  implicit val genString: Generator[String] = Generator.create {
//    case ConstRule(value) => value
//    case RandomRule()     => Random.alphanumeric.take(10).mkString
//  }

  def main(args: Array[String]): Unit = {
    println(
      Generator[User]
        .func(age = ConstRule(45), sex = Some(RandomRule))
    )
  }
}
