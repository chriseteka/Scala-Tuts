package lectures

import scala.language.postfixOps

object MethodNotations extends App {

  class Person(val name: String, val favMovie: String, val age: Int = 0){
    def likes(movie: String): Boolean = movie equalsIgnoreCase favMovie
    def +(person: Person): String = s"${this.name} hangs out with ${person.name}"
    def +(nickName: String): Person = new Person(s"$name ($nickName)", favMovie)
    def unary_! : String = s"What the heck, is this not: $name?"
    def unary_+ : Person = new Person(name, favMovie, age + 1)
    def isAlive: Boolean = true
    def learns(sub: String): String = s"$name learns $sub"
    def learnsScala: String = learns("Scala")
    def apply(): String = s"Hello, my name is $name, and i love $favMovie"
    def apply(i: Int): String = s"$name watched $favMovie $i times"

    override def toString: String = s"Person[name=$name, favMovie=$favMovie, age=${if (age == 0) null else age}]"
  }

  val mary = new Person("Mary", "Nigerian Movie")
  val victor = new Person("Victor", "Scam movie")

  //INFIX NOTATION
  println(mary likes "Nigerian Movie")
  println(mary + victor)
  //PREFIX NOTATION
  println(!mary)
  //POSTFIX NOTATION
  println(mary isAlive)
  //The method apply allows us call an instance of a class as if it were a method
  println(mary())


  //EXERCISES
  println((mary + "Slay mama")())
  println(+(+mary))
  println(mary learns "English Language")
  println(mary learnsScala)
  println(mary(2))

  //Testing out how to write companion objects, creating classes without the new keyword
  val Osita = Companion("Osita@gmail.baby,sly", 25, "SlyMan")
  println(Osita)
}
