package dev.aliakovl.gin

trait Random {
  def nextBoolean(): Boolean

  def nextBytes(bytes: Array[Byte]): Unit

  def nextBytes(n: Int): Array[Byte]

  def nextDouble(): Double

  def between(minInclusive: Double, maxExclusive: Double): Double

  def nextFloat(): Float

  def between(minInclusive: Float, maxExclusive: Float): Float

  def nextInt(): Int

  def nextInt(n: Int): Int

  def between(minInclusive: Int, maxExclusive: Int): Int

  def nextLong(): Long

  def nextLong(n: Long): Long

  def between(minInclusive: Long, maxExclusive: Long): Long

  def nextString(length: Int): String

  def nextPrintableChar(): Char

  def alphanumeric: LazyList[Char]
}

object Random {
  class Unsafe(override val self: java.util.Random) extends scala.util.Random with Random {
    def this(seed: Long) = this(new java.util.Random(seed))

    def this(seed: Int) = this(seed.toLong)

    def this() = this(new java.util.Random())
  }

  object Unsafe extends Unsafe()
}
