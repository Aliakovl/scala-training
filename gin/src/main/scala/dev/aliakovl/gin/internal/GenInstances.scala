package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Gen

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, LocalDateTime, YearMonth}
import java.util.UUID
import scala.util.Random

trait GenInstances {
  implicit val uuidGen: Gen[UUID] = Gen(UUID.randomUUID())
  implicit val stringGen: Gen[String] = Gen.alphanumeric(10)
  implicit val charGen: Gen[Char] = Gen(Random.nextPrintableChar())
  implicit val instantGen: Gen[Instant] = Gen(Instant.now().truncatedTo(ChronoUnit.MILLIS))
  implicit val intGen: Gen[Int] = Gen(Random.nextInt())
  implicit val longGen: Gen[Long] = Gen(Random.nextLong())
  implicit val doubleGen: Gen[Double] = Gen(Random.nextDouble())
  implicit val booleanGen: Gen[Boolean] = Gen(Random.nextBoolean())
  implicit val bigDecimalGen: Gen[BigDecimal] = Gen(BigDecimal.valueOf(Random.nextDouble()))
  implicit val localDateGen: Gen[LocalDate] = Gen(LocalDate.now())
  implicit val localDateTimeGen: Gen[LocalDateTime] = Gen(LocalDateTime.now())
  implicit val yearMonthGen: Gen[YearMonth] = Gen(YearMonth.now())
  implicit def defaultIterableGen2d[A: Gen, B: Gen]: Gen[Map[A, B]] =
    Gen.many2[Map](5).make[A, B]
}
