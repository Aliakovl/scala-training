package dev.aliakovl.gin

object Main {
  def main(args: Array[String]): Unit = {
    println(
      Random.custom[(Int, String)].random()
    )

    println(
      Random
        .custom[Option[Int]]
        .specifyConst(_.when[Some[Int]].value)(-1)
        .random()
    )
  }
}
