package dev.aliakovl.gin

import dev.aliakovl.gin.Random.{alphanumeric, const}

import java.time.{Instant, LocalDate, LocalDateTime, YearMonth}
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.collection.{Factory, Iterable}
import scala.util.{Random => ScalaRandom}

trait RandomInstances {
  implicit val uuidRandom: Random[UUID] = Random(UUID.randomUUID())
  implicit val stringRandom: Random[String] = alphanumeric(10)
  implicit val charRandom: Random[Char] = Random(ScalaRandom.nextPrintableChar())
  implicit val instantRandom: Random[Instant] = Random(
    Instant.now().truncatedTo(ChronoUnit.MILLIS)
  )
  implicit val intRandom: Random[Int] = Random(ScalaRandom.nextInt())
  implicit val longRandom: Random[Long] = Random(ScalaRandom.nextLong())
  implicit val doubleRandom: Random[Double] = Random(ScalaRandom.nextDouble())
  implicit val booleanRandom: Random[Boolean] = Random(ScalaRandom.nextBoolean())
  implicit val bigDecimalRandom: Random[BigDecimal] = Random(BigDecimal.valueOf(ScalaRandom.nextDouble()))
  implicit val localDateRandom: Random[LocalDate] = Random(LocalDate.now())
  implicit val localDateTimeRandom: Random[LocalDateTime] = Random(LocalDateTime.now())
  implicit val yearMonthRandom: Random[YearMonth] = Random(YearMonth.now())
  implicit def defaultIterableRandom2d[
      A: Random,
      B: Random,
      M[K, V] <: Iterable[(K, V)]
  ](implicit
      f: Factory[(A, B), M[A, B]]
  ): Random[M[A, B]] = Random.many2[M](5).make[A, B]
}
