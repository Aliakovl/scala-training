package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Random

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, LocalDateTime, YearMonth}
import java.util.UUID
import scala.util.{Random => ScalaRandom}

trait RandomInstances {
  implicit val uuidRandom: Random[UUID] = Random(UUID.randomUUID())
  implicit val stringRandom: Random[String] = Random.alphanumeric(10)
  implicit val charRandom: Random[Char] = Random(ScalaRandom.nextPrintableChar())
  implicit val instantRandom: Random[Instant] = Random(Instant.now().truncatedTo(ChronoUnit.MILLIS))
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
  ]: Random[Map[A, B]] = Random.many2[Map](5).make[A, B]
}
