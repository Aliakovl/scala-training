package dev.aliakovl.gin

case class TestCase[A](gen: Gen[A]) {
  def apply(checks: Seq[A] => Any): Unit = {
    checks(gen.many[Seq](100).runWithSeed(599311303609L))
  }
}
