package dev.aliakovl.recursion.fix

sealed trait Fix[+F[_]]
case class In[F[_]](out: F[Fix[F]]) extends Fix[F]
