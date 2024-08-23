package dev.aliakovl.gin

trait GenOps[A] {
  def random: Random[A]
  def debug: String
}
