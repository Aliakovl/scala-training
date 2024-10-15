package dev.aliakovl.gin

case class TestCase[A](gen: Gen[A]) {
  def apply(checks: List[A] => Any): Unit = {
    checks(gen.many[List](100).run())
  }
}
