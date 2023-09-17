package dev.aliakovl.management

import zio.Console._
import zio._

object Main extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = printLine("management")
}
