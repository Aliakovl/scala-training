import optics.Lens

import scala.language.existentials

object TestLens {
  case class Address(strNumber: Int, street: String)
  case class Person(name: String, age: Int, address: Address)

  val address: Address = Address(10, "High Street")
  val john: Person = Person("John", 20, address)

  val personAddressLens: Lens[Person, Address] =
    Lens[Person, Address](_.address) { address => person =>
      person.copy(address = address)
    }

  val addressStreetLens: Lens[Address, String] =
    Lens[Address, String](_.street) { street => address =>
      address.copy(street = street)
    }

  val personStreetLens: Lens[Person, String] =
    personAddressLens ^|-> addressStreetLens

  def main(args: Array[String]): Unit = {
    assert(personAddressLens.get(john) == address)
    assert(personStreetLens.set("New Street")(john) == Person("John", 20, Address(10, "New Street")))
    assert(personStreetLens.modify(_.reverse)(john) == Person("John", 20, Address(10, "teertS hgiH")))
  }
}
