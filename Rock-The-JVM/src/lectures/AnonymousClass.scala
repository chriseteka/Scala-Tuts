package lectures

object AnonymousClass extends App {

  abstract class Animal{
    def eat: Unit
  }

  trait AnimalBehaviour{
    def talk: Boolean
  }

  class Person(name: String){
    def walks: Boolean = true
  }

  //Anonymous class on an abstract class
  val funnyAnimal = new Animal {
    override def eat: Unit = println("Hahahahhaha")
  }

  //Anonymous class on a trait
  val parrot = new AnimalBehaviour {
    override def talk: Boolean = true
  }

  //Anonymous class on a normal class
  val cripple = new Person("cripple"){
    override def walks: Boolean = false
  }

  //PS: 1. When instantiating an abstract in an anonymous manner, be sure to provide implementations
  // to all its abstract methods.
  //2. If you are instantiating a class that has parameters in an anonymous manner,
  // be sure to specify these params during instantiation
}
