package dev.aliakovl.gin.macros

import dev.aliakovl.gin.Random

trait GenOps[A] {
  def random: Random[A]
  def debug: String
}
