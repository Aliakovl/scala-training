package dev.aliakovl.core.data

import dev.aliakovl.core.data.HList.HNil

import scala.concurrent.ExecutionContext.parasitic
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}

object HListSpec {
  val list: Int :: Double :: String :: Boolean :: HNil =
    1 :: 1.0 :: "One" :: false :: HNil

  object Green
  object Red
  object Yellow

  type Light = Green.type :+: Red.type :+: Yellow.type :+: CNil

  val light: Light = Coproduct[Light](Red)

  def main(args: Array[String]): Unit = {
    assert(list.head == 1)
    assert(list.tail == 1.0 :: "One" :: false :: HNil)
    println(light.select[Red.type])
    println(light.select[Green.type])
    println(light.select[Yellow.type])
    println(light.select[3])

    lazy val fib: LazyList[Long] =
      0L #:: 1L #:: fib.lazyZip(fib.tail).map { case (a, b) => a + b }

    fib.take(100).toList

    val listView = List(1, 2, 3).view
      .map { i =>
        println(s"1 - $i")
        i + 1
      }
      .map { i =>
        println(s"2 - $i")
        i + 1
      }
    listView.toList

//    import scala.concurrent.ExecutionContext.Implicits.global

//    val r: Future[Int] = for {
//      a <- Future.successful(3)
//      b <- Future.successful(45)
//      if a + b < 5
//    } yield a + b
//
//    r.onComplete { tr =>
//      println(tr)
//    }

//    Await.ready(r, 1.minute)

//    val p = Promise[Nothing]()
//    val f = p.future
//
//    Await.ready(f, 5.seconds)

    def g[A, B](future: Future[A])(f: A => B): Future[B] = {
      future.map(f)(parasitic)
    }

    val r: Future[Int] = g(Future.successful[Int] {
      Thread.sleep(3.second.toMillis)
      5
    })(i => i + 1)

    println(r)

  }
}
