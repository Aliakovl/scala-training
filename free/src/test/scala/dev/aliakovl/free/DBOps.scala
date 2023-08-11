package dev.aliakovl.free

import dev.aliakovl.core.~>

import scala.collection.mutable

sealed trait DBOps[A]
final case class Create[A](key: String, value: A) extends DBOps[Unit]
final case class Read[A](key: String) extends DBOps[A]

object DBOps {
  type DBMonad[A] = Free[DBOps, A]

  def create[A](key: String, value: A): DBMonad[Unit] = {
    Free.liftM[DBOps, Unit](Create(key, value))
  }

  def read[A](key: String): DBMonad[A] = Free.liftM[DBOps, A](Read(key))

  private val store = new mutable.HashMap[String, Any]()

  implicit val natTrans: DBOps ~> IO = new (DBOps ~> IO) {
    override def apply[A](fa: DBOps[A]): IO[A] = fa match {
      case Create(key, value) =>
        IO.defer {
          println(s"create ${value} in ${key}")
          store += (key -> value)
          ()
        }
      case Read(key) =>
        IO.defer {
          println(s"select * from ${key}")
          store(key).asInstanceOf[A]
        }
    }
  }
}
