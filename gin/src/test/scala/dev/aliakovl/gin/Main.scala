package dev.aliakovl.gin

object Main {
  def main(args: Array[String]): Unit = {
    println(
      Gen.custom[(Int, String)].make()
    )

    println(
      Gen
        .custom[Option[Int]]
        .specifyConst(_.when[Some[Int]].value)(-1)
        .make()
    )
  }
}
