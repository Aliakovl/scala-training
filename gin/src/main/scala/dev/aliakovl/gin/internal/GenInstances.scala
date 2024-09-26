package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Gen

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, LocalDateTime, YearMonth}
import java.util.UUID

trait GenInstances {
  implicit val uuidGen: Gen[UUID] = Gen { random =>
    new UUID(
      (random.nextLong() & 0xffff0fff) | 0x00004000,
      (random.nextLong() & 0x3fffffffffffffffL) | 8000000000000000L
    )
  }
  implicit val stringGen: Gen[String] = Gen.alphanumeric(10)
  implicit val charGen: Gen[Char] = Gen(_.nextPrintableChar())
  implicit val instantGen: Gen[Instant] = Gen.make(
    Instant.now().truncatedTo(ChronoUnit.MILLIS)
  )
  implicit val shortGen: Gen[Short] = Gen { random =>
    val a = random.nextBytes(2)
    ((a(0) << 8).toShort | a(1).toShort).toShort
  }
  implicit val intGen: Gen[Int] = Gen(_.nextInt())
  implicit val byteGen: Gen[Byte] = Gen(_.nextBytes(1)(0))
  implicit val longGen: Gen[Long] = Gen(_.nextLong())
  implicit val doubleGen: Gen[Double] = Gen(_.nextDouble())
  implicit val floatGen: Gen[Float] = Gen(_.nextFloat())
  implicit val booleanGen: Gen[Boolean] = Gen(_.nextBoolean())
  implicit val bigDecimalGen: Gen[BigDecimal] = Gen { random =>
    BigDecimal.valueOf(random.nextDouble())
  }
  implicit val bigIntGen: Gen[BigInt] = Gen { random =>
    BigInt(random.nextInt())
  }
  implicit val localDateGen: Gen[LocalDate] = Gen.make(LocalDate.EPOCH)
  implicit val localDateTimeGen: Gen[LocalDateTime] =
    Gen.make(LocalDateTime.now())
  implicit val yearMonthGen: Gen[YearMonth] = Gen.make(YearMonth.now())
  implicit def defaultIterableGen2d[A: Gen, B: Gen]: Gen[Map[A, B]] = Gen
    .between(0, 10)
    .flatMap(Gen.product[A, B](Gen.random[A], Gen.random[B]).toMap(_))
}
