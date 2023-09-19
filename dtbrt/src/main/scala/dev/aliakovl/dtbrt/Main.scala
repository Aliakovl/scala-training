package dev.aliakovl.dtbrt

case class Person(name: String, age: Int, isAlive: Boolean, other: Double = 5.5)
case class User(name: String, isAlive: Boolean, age: Int, fanciness: Double)

def notifyUser(user: User): Unit = println(s"Hey, ${user.name} (${user.age} years old) (alive: ${user.isAlive})!")

def person2User(person: Person): User =
  person.into[User]
    .withConstant(_.fanciness, 99.9)
    .transform

@main
def run(): Unit =
  val person: Person = Person("Aleksei", 25, true)
  val user: User = person2User(person)
  notifyUser(user)
