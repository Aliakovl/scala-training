package dev.aliakovl.gin

import java.time.{Instant, LocalDate, LocalDateTime, YearMonth}
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.collection.{Factory, Iterable}
import scala.util.{Random => ScalaRandom}

trait GeneratorInstances {
  implicit val uuidGenerator: Random[UUID] = () => UUID.randomUUID()
  implicit val stringGenerator: Random[String] = () =>
    ScalaRandom.alphanumeric.take(10).mkString
  implicit val char: Random[Char] = () => ScalaRandom.nextPrintableChar()
  implicit val instantGenerator: Random[Instant] = () =>
    Instant.now().truncatedTo(ChronoUnit.MILLIS)
  implicit val intGenerator: Random[Int] = () => ScalaRandom.nextInt()
  implicit val longGenerator: Random[Long] = () => ScalaRandom.nextLong()
  implicit val doubleGenerator: Random[Double] = () => ScalaRandom.nextDouble()
  implicit val booleanGenerator: Random[Boolean] = () =>
    ScalaRandom.nextBoolean()
  implicit val bigDecimalGenerator: Random[BigDecimal] = () =>
    BigDecimal.valueOf(ScalaRandom.nextDouble())
  implicit val localDateGenerator: Random[LocalDate] = () => LocalDate.now()
  implicit val localDateTimeGenerator: Random[LocalDateTime] = () =>
    LocalDateTime.now()
  implicit val yearMonthGenerator: Random[YearMonth] = () => YearMonth.now()
  implicit def defaultIterableGenerator[A: Random, C[E] <: IterableOnce[E]](
      implicit f: Factory[A, C[A]]
  ): Random[C[A]] = Random.many[C](5).make[A]
  implicit def defaultIterableGenerator2d[
      A: Random,
      B: Random,
      M[K, V] <: Iterable[(K, V)]
  ](implicit
      f: Factory[(A, B), M[A, B]]
  ): Random[M[A, B]] = Random.many2[M](5).make[A, B]

  def randomFrom[A](xs: IterableOnce[A]): Random[A] = () =>
    ScalaRandom.shuffle(xs.iterator.toIndexedSeq).head

  def enumerationGenerator[E <: Enumeration](
      enumeration: E
  ): Random[E#Value] =
    randomFrom(enumeration.values)
}
