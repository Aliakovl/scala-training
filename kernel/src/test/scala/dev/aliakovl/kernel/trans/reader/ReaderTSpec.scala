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

  val program: ZIO[String, LengthException, Int] = for {
    n <- asks[M, String, Int](_.length)
    _ <- if n > 5 then
      ReaderT.lift[M, String, Unit](throwE(ToLong))
    else if n < 1 then
      ReaderT.lift[M, String, Unit](throwE(ToShort))
    else
      ReaderT.pure[M, String, Unit](())
  } yield n

  val result: String = ReaderT[[T] =>> Except[Nothing, T], String, String] { r =>
    program.map(_.toString).runReaderT(r).catchE(e => ExceptT.pure(e.toString))
  }.runReaderT(str).runExceptT.runId.getSafe

  def main(args: Array[String]): Unit = {
    println(result)
  }
}
