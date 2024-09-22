package dev.aliakovl.gin.internal

import dev.aliakovl.gin.Gen

trait FunctionConstructor[In] {
  type Out
  def apply(in: In): Out
}

object FunctionConstructor {
  type Aux[In, Out0] = FunctionConstructor[In] { type Out = Out0 }

  implicit def function0Constructor[A]: FunctionConstructor.Aux[() => A, Gen[A]] =
    new FunctionConstructor[() => A] {
      override type Out = Gen[A]
      override def apply(in: () => A): Gen[A] = Gen(in())
    }

  implicit def function1Constructor[T1: Gen, A]: FunctionConstructor.Aux[T1 => A, Gen[A]] =
    new FunctionConstructor[T1 => A] {
      override type Out = Gen[A]
      override def apply(in: T1 => A): Gen[A] = Gen(
        in(
          Gen.random[T1].apply()
        )
      )
    }

  implicit def function2Constructor[T1: Gen, T2: Gen, A]: FunctionConstructor.Aux[(T1, T2) => A, Gen[A]] =
    new FunctionConstructor[(T1, T2) => A] {
      override type Out = Gen[A]
      override def apply(in: (T1, T2) => A): Gen[A] = Gen(
        in(
          Gen.random[T1].apply(),
          Gen.random[T2].apply()
        )
      )
    }
}
