package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{alphanumeric, random}

import java.time.{Instant, LocalDate, LocalDateTime, YearMonth}
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.collection.{Factory, Iterable}
import scala.util.{Random => ScalaRandom}

trait GeneratorInstances {
  implicit val uuidGenerator: Random[UUID] = random(UUID.randomUUID())
  implicit val stringGenerator: Random[String] = alphanumeric(10)
  implicit val char: Random[Char] = random(ScalaRandom.nextPrintableChar())
  implicit val instantGenerator: Random[Instant] = random(
    Instant.now().truncatedTo(ChronoUnit.MILLIS)
  )
  implicit val intGenerator: Random[Int] = random(ScalaRandom.nextInt())
  implicit val longGenerator: Random[Long] = random(ScalaRandom.nextLong())
  implicit val doubleGenerator: Random[Double] = random(ScalaRandom.nextDouble())
  implicit val booleanGenerator: Random[Boolean] = random(ScalaRandom.nextBoolean())
  implicit val bigDecimalGenerator: Random[BigDecimal] = random(BigDecimal.valueOf(ScalaRandom.nextDouble()))
  implicit val localDateGenerator: Random[LocalDate] = random(LocalDate.now())
  implicit val localDateTimeGenerator: Random[LocalDateTime] = random(LocalDateTime.now())
  implicit val yearMonthGenerator: Random[YearMonth] = random(YearMonth.now())
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

  def randomFrom[A](xs: IterableOnce[A]): Random[A] = random(ScalaRandom.shuffle(xs.iterator.toIndexedSeq).head)

  def enumerationGenerator[E <: Enumeration](
      enumeration: E
  ): Random[E#Value] =
    randomFrom(enumeration.values)
}
