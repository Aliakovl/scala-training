package dev.aliakovl.awk.backend

trait AwkQuery[T]:
  def map[R](f: T => R): AwkQuery[R] =
    throw new IllegalAccessError()

object AwkQuery:
  def apply[T] = new AwkQuery[T]() {}
