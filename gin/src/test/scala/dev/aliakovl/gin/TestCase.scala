package dev.aliakovl.gin

case class TestCase[A](gen: Gen[A]) {
  def apply[U](checks: List[A] => U): U = {
    checks(gen.many[List](100).run())
  }
}
