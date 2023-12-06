package dev.aliakovl.recursion.fix

sealed trait Fix[F[_]]:
  def out: F[Fix[F]]

case class In[F[_]](override val out: F[Fix[F]]) extends Fix[F]
