package dev.aliakovl.gin

case class TestCase[A](gen: Gen[A]) {
  private val testSuites = gen.many[Seq](100).runWithSeed(599311303609L)

  def apply(checks: Seq[A] => Any): Unit = {
    checks(testSuites)
  }

  def foreach(check: A => Any): Unit = {
    testSuites.foreach(check)
  }
}
