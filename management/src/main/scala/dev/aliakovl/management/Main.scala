package dev.aliakovl.management

import dev.aliakovl.management
import zio._

import java.io.RandomAccessFile
import java.nio.channels.FileChannel

object Main extends ZIOApp {

  val makeDb: ZIO[Scope, Throwable, DB] =
    ZIO.acquireRelease(management.DB.open())(_.close())

  def makeServer(db: DB): ZIO[Scope, Throwable, Server] =
    ZIO.acquireRelease(Server.open(db))(_.close())

  val file: ZIO[Scope, Throwable, FileChannel] = ZIO.acquireRelease(
    ZIO.attempt(new RandomAccessFile("file", "rw")).mapAttempt(_.getChannel)
  )(a => ZIO.succeed(a.close()))

  override def run = {
        for {
          _ <- ZIO.serviceWithZIO[Server](_.push("12345"))
        } yield ()
      }
      .onInterrupt(_ => ZIO.succeed(println("stooooped")))
      .exitCode
      .debug


  override val bootstrap: ZLayer[ZIOAppArgs, Throwable, Server] = ZLayer.scoped(
    for {
      db <- makeDb
      server <- makeServer(db)
    } yield server
  )

  override val environmentTag: EnvironmentTag[Server] = EnvironmentTag[Server]

  override type Environment = Server
}

trait Closable[R] {
  def close(): UIO[Unit]
}

class DB extends Closable[DB] {
  def get(message: String): Task[String] = ZIO.succeed(message.reverse)
  def close(): UIO[Unit] = zio.Console.printLine("db close").!
}

object DB {
  def open(): Task[DB] = zio.Console.printLine("db open") *> ZIO.succeed(new DB)
}

class Server(db: DB) extends Closable[Server] {
  def push(message: String): Task[Unit] = db.get(message).tap(ZIO.log(_)).unit

  override def close(): UIO[Unit] = zio.Console.printLine("server close").!
}

object Server {
  def open(db: DB): Task[Server] =
    zio.Console.printLine("server open") *> ZIO.succeed(new Server(db))
}
