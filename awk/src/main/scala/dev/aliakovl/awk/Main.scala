package dev.aliakovl.awk

import dev.aliakovl.awk.backend.ASTCompiler.compile
import dev.aliakovl.awk.backend.AwkQuery
import dev.aliakovl.awk.frontend.Quoted.quote

object Main {

  case class Data(
      code: String,
      name: String,
      description: String,
      quantity: String
  )

  val q = quote {
    AwkQuery[Data].map(x => (x.name, x.quantity)).map(x => (x._2, x._1))
  }

  def main(args: Array[String]): Unit = {
    println(compile(q.expr))
  }
}
