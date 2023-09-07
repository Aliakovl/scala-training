package dev.aliakovl.kernel.trans.except

import dev.aliakovl.kernel.trans.except.ExceptT
import dev.aliakovl.kernel.given

object ExceptTSpec {
  val someRight_3: ExceptT[Option, String, Int] = ExceptT.right(Some(3))
  val someRight_5: ExceptT[Option, String, Long] = ExceptT.right(Some(5))
  val someLeft: ExceptT[Option, String, Unit] = ExceptT.left(Some("text"))
  val noneRight: ExceptT[Option, String, Int] = ExceptT.right(None)
  val noneLeft: ExceptT[Option, String, Int] = ExceptT.left(None)

  def main(args: Array[String]): Unit = {
    {
      val res = for {
        a <- someRight_3
        b <- someRight_5
      } yield a + b

      assert(res == ExceptT(Some(Right(8))))
    }

    {
      val res = for {
        a <- someRight_3
        b <- someLeft
      } yield s"$a$b"

      assert(res == ExceptT(Some(Left("text"))))
    }

    {
      val res = for {
        a <- someRight_3
        b <- noneRight
      } yield s"$a$b"

      assert(res == ExceptT(None))
    }

    {
      val res = for {
        a <- someRight_3
        b <- noneLeft
      } yield s"$a$b"

      assert(res == ExceptT(None))
    }

    {
      val res = for {
        a <- someLeft
        b <- noneLeft
      } yield s"$a$b"

      assert(res == someLeft)
    }

    {
      val res = for {
        a <- noneLeft
        b <- someLeft
      } yield s"$a$b"

      assert(res == noneLeft)
    }

    {
      val res = someLeft.withExceptT(a => s"$a$a")
      assert(res == ExceptT(Some(Left("texttext"))))
    }

  }

}
