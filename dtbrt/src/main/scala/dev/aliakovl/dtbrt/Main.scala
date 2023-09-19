package dev.aliakovl.dtbrt

case class Person(name: String, age: Int, isAlive: Boolean, dullness: Double)
case class User(name: String, isAlive: Boolean, age: Int, fanciness: Double)

def notifyUser(user: User): Unit = println(s"Hey, ${user.name} (${user.age} years old) (alive: ${user.isAlive}) (fanciness: ${user.fanciness}) !")

def person2User(person: Person): User =
  person.into[User]
    .withConstant(_.age, 6)
    .withComputed(_.fanciness, _.dullness * 10)
    .transform

@main
def run(): Unit =
  val person: Person = Person("Aleksei", 25, true, 55)
  val user: User = person2User(person)
  notifyUser(user)
