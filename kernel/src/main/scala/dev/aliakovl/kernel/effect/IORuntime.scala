package dev.aliakovl.kernel.effect

import scala.collection.mutable

class IORuntime(io: IO[Any]):
  type Erased = IO[Any]
  type Cont = Any => Erased

  var currentIO: IO[Any] = io
  var loop = true

  val stack = new mutable.Stack[Cont]()

  def complite(value: Any)(callback: Any => Unit): Unit = {
    if stack.isEmpty then
      loop = false
      callback(value)
    else
      val cont = stack.pop()
      currentIO = cont(value)
  }

  final def runLoop(callback: Any => Unit): Unit = while (loop) {
    currentIO match
      case IO.Success(value) => complite(value())(callback)
      case IO.FlatMap(io, f) =>
        currentIO = io
        stack.push(f.asInstanceOf[Cont])
  }

  def runUnsafe(callback: Any => Unit): Unit =
    runLoop(callback)
