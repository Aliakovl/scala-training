package dev.aliakovl.gin

object Main {
  def main(args: Array[String]): Unit = {
    println(
      Gen[(Int, String)].random()
    )

    println(
      Gen[Option[Int]].specifyConst(_.when[Some[Int]].value)(-1).random()
    )
  }
}
