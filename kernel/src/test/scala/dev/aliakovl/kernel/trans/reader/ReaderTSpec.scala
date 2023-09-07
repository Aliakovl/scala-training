package dev.aliakovl.kernel.trans.reader

import dev.aliakovl.kernel.data.Id
import dev.aliakovl.kernel.trans.reader.ReaderT
import dev.aliakovl.kernel.trans.except.{Except, ExceptT}

enum LengthException:
  case ToShort, ToLong

type ZIO[R, E, A] = ReaderT[[T] =>> ExceptT[Id, E, T], R, A]

extension[A] (either: Either[Nothing, A])
  def getSafe: A = either.toOption.get

object ReaderTSpec {
  import ReaderT.*
  import ExceptT.*
  import LengthException.*

  val str = "Hello"

  type M[T] = Except[LengthException, T]

  def rightString(min: Int, max: Int): ZIO[String, LengthException, Int] = for {
    n <- ReaderT.asks[M, String, Int](_.length)
    _ <- if n > max then
      ReaderT.lift[M, String, Unit](throwE(ToLong))
    else if n < min then
      ReaderT.lift[M, String, Unit](throwE(ToShort))
    else
      ReaderT.pure[M, String, Unit](())
  } yield n

  val program: ZIO[String, Nothing, String] = ReaderT[[T] =>> Except[Nothing, T], String, String] { r =>
    rightString(1, 6).map(_.toString).runReaderT(r).catchE(e => ExceptT.pure(e.toString))
  }

  def main(args: Array[String]): Unit = {
    val result = program.runReaderT(str).runExceptT.runId.getSafe
    println(result)
  }
}
