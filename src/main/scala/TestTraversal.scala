import core.Traverse._
import optics.Traversal

object TestTraversal {
  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val a = List(3, 5, 4, 6, 5, 4, 5)
    val traversalList: Traversal[List[Int], Int] =
      Traversal.fromTraverse[List, Int]

    println(traversalList.set(4)(a))
    println(traversalList.modify(_ + 1)(a))

    val pointTraversal: Traversal[Point, Int] =
      Traversal.apply2[Point, Int](_.x, _.y) { (x, y, p) =>
        p.copy(x = x, y = y)
      }

    println(pointTraversal.modify(_ + 1)(Point(4, 5)))
  }
}
