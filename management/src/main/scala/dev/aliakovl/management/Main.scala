package dev.aliakovl.management

import dev.aliakovl.management
import zio._
import zio.stream.ZStream

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Paths, StandardOpenOption}

object Main extends ZIOAppDefault {

  val makeDb: ZIO[Scope, Throwable, DB] = ZIO.acquireRelease(management.DB.open())(_.close())

  def makeServer(db: DB): ZIO[Scope, Throwable, Server] = ZIO.acquireRelease(Server.open(db))(_.close())

  val file: ZIO[Scope, Throwable, FileChannel] = ZIO.acquireRelease(ZIO.attempt(new RandomAccessFile("file", "rw")).mapAttempt(_.getChannel))(a => ZIO.succeed(a.close()))

  override def run: ZIO[Any with ZIOAppArgs with Scope, Throwable, ExitCode] = ZIO.scoped {
    for {
      db <- makeDb
      server <- makeServer(db)
      _ <- zio.Console.printLine("do something")
      c <- file
      lock = c.lock(0, 1073742335, false)
      _ <- zio.Console.printLine(lock.isValid)
      _ <- ZIO.attempt(lock.channel().write(ByteBuffer.wrap(Array('.').map(_.toByte)))).repeat(Schedule.fixed(1.second) && Schedule.recurs(100))
      _ <- ZIO.attempt(lock.release())
      _ <- ZIO.never
      _ <- zio.Console.printLine("do something 2")
    } yield ()
  }.onInterrupt(_ => ZIO.succeed(println("stooooped"))).exitCode.debug
}

trait Closable[R] {
  def close(): UIO[Unit]
}

class DB extends Closable[DB] {
  def close(): UIO[Unit] = zio.Console.printLine("db close").!
}

object DB {
  def open(): Task[DB] = zio.Console.printLine("db open") *> ZIO.succeed(new DB)
}

class Server(db: DB) extends Closable[Server] {
  override def close(): UIO[Unit] = zio.Console.printLine("server close").!
}

object Server {
  def open(db: DB):Task[Server] = zio.Console.printLine("server open") *> ZIO.succeed(new Server(db))
}
