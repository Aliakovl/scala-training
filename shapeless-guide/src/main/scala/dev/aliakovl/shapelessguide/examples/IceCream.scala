package dev.aliakovl.shapelessguide.examples


case class IceCream(name: String, numCherries: Int, inCone: Boolean)

object IceCream {

//  import dev.aliakovl.shapelessguide.examples.CsvEncoder.createEncoder
//  import shapeless.Generic
//
//  implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
//    val gen = Generic[IceCream]
//    val enc = CsvEncoder[gen.Repr]
//    createEncoder { iceCream =>
//      enc.encode(gen.to(iceCream))
//    }
//  }
}
