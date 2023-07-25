package dev.aliakovl.optics

import dev.aliakovl.core.Traverse._

object TestTraversal {
  case class Point(x: Int, y: Int)

  val list: List[Int] = List(3, 5, 4, 6, 5, 4, 5)
  val traversalList: Traversal[List[Int], Int] =
    Traversal.fromTraverse[List, Int]

  val pointTraversal: Traversal[Point, Int] =
    Traversal.apply2[Point, Int](_.x, _.y) { (x, y, p) =>
      p.copy(x = x, y = y)
    }

  def main(args: Array[String]): Unit = {
    assert(traversalList.set(4)(list) == list.map(_ => 4))
    assert(traversalList.modify(_ + 1)(list) == list.map(_ + 1))
    assert(pointTraversal.modify(_ + 1)(Point(4, 5)) == Point(5, 6))
    println("TestTraversal")
  }
}
