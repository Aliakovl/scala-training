package dev.aliakovl

package object core {
  type ~>[F[_], G[_]] = FunctionK[F, G]
}
