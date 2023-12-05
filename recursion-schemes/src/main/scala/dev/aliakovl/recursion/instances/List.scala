package dev.aliakovl.recursion.instances

import dev.aliakovl.kernel.Functor
import dev.aliakovl.recursion.fix.*
import dev.aliakovl.recursion.instances.*
import dev.aliakovl.recursion.schemes.Catamorphism.*
import dev.aliakovl.recursion.schemes.Hylomorphism.*

sealed trait L[+A, +R]
object NL extends L[Nothing, Nothing]
case class CL[+A, +R](v: A, r: R) extends L[A, R]

object L:
  given[O]: Functor[[R] =>> L[O, R]] with
    extension[A, FF[T] <: L[O, T]] (fa: FF[A])
      def map[B](f: A => B): L[O, B] = fa match {
        case NL => NL
        case cl: CL[O, A] => CL(cl.v, f(cl.r))
      }

type List[A] = Fix[[R] =>> L[A, R]]

object ListMain:
  def factorial(n: Int): Int = {
    hylo[[R] =>> L[Int, R], Int, Int] {
      case NL => 1
      case CL(a, r) => a * r
    } {
      case 0 => NL
      case i => CL(i, i - 1)
    }(n)
  }

  val list: List[Int] = In(CL(1, In(CL(2, In(CL(3, In(CL(4, In(CL(5, In(NL)))))))))))

  def main(args: Array[String]): Unit =
    println(factorial(5))
    println(
      List(1,2,3,4,5).foldRight(List.empty)((a, r) => a :: r)
    )
    println(
      list.cata[scala.collection.immutable.List[Int]] {
        case NL => List.empty[Int]
        case CL(a, r) => a :: r
      }
    )
