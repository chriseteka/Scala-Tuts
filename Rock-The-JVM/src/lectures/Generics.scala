package lectures

object Generics extends App {

//  class Car
  class Animal
  class Dog extends Animal
  class Cat extends Animal

  //Question: Does List[Dog] extend List[Animal]
  //1. Yes, this is a property known as COVARIANCE, hence a generic list for this property is given:
  class CovariantList[+A]{
//    def add(elem: A): CovariantList[A] = ??? // This wont work, because method `add` has been asked to collect all sub and super
    // types of `A` whereas we are creating a list of covariant type (subtype of A).
    // A way out is to allow method `add` to collect, all types `B` that is a super type of `A`, and then return a list
    // of the super class of `A` otherwise a list of `B`, we achieve this by using bounded types
    def add[B >: A](elem: B): CovariantList[B] = ???
  } //Notice the `plus` sign before the generic type `A`, it allows the polymorphic property
  val animalList: CovariantList[Animal] = new CovariantList[Cat] // This is likened to the code below
  val animal: Animal = new Cat
  //Problem with this now is, can we add a `Dog` to the List created since Dog is a subclass of Animal??? Difficult to say
  // Answer however is YES, but then we must return a list of animal in the end, and no longer a list of cats


  //2. No, this is a property known as INVARIANCE, where the list of a subclass does not extend the list of its super type
  //It is defined thus:
  class InvariantList[A] // Notice there is not sign prefixed to the generic type `A`, hence
  // val animalList2: InvariantList[Animal] = new InvariantList[Cat] // Cannot work in a polymorphic manner
  val animalList2: InvariantList[Animal] = new InvariantList[Animal] // This however is allowed
  //This however allows any object created from the subclasses of `Animal` to be added to the invariant list

  //3. Hell No, this is a property known as CONTRAVARIANCE, list of a subtype never extends the list of its super type
  //It is defined thus:
  class ContravariantList[-A] //Observe the use of `minus` before the generic type `A`
  val animalList3: ContravariantList[Cat] = new ContravariantList[Animal] // works in a polymorphic manner,
  // but in an opposite direction, such that the list of a super class extends the list of its sub type,
  // making the subtype act like a parent of its super class. Although for a list, this doesn't make sense.

  //Overtime, we will see the application of these and how they are important
  //Consider an instance where you have a class `Trainer` to represent someone who trains animals
  class Trainer[-A] //It will be better to use contravariant here, so that we can cover a wide range of trainers,
  // given any subtype. Say we have a cat to train and we are looking for a trainer `catTrainer`
  val catTrainer: Trainer[Cat] = new Trainer[Animal] // It will make more sense to not just look for a catTrainer only,
  // but all other trainer that trains animals in general, this is the importance of contravariance, more is on its way

  // BOUNDED TYPES: This allows us to use generic class only for certain types which are either subclasses of this given
  //type or superclasses of the given type, examples and further explanation are:
  class Cage[A <: Animal](animal: Animal) // This is an UPPER BOUNDED TYPE ON TYPE ANIMAL
  // The above states that class cage only accepts class types `A` that are subclasses (the symbol <:) of type Animal, hence:
  val cage = new Cage(new Dog) //This is a valid code
  // val anotherCage = new Cage(new Car) // This is an invalid code.

  class Home[B >: Animal] // This is a LOWER BOUNDED TYPE ON TYPE ANIMAL
  // The above states that class Home only accepts class types `B` that are superclasses (the symbol >:) of type Animal, hence:

  //The bounded types helps us to solve the problem presented in COVARIANCE. This problem is:
  val cov: CovariantList[Animal] = new CovariantList[Cat] // Given a variable cov, which is a list of Animal,
  // but was passed a list of cat.
  //Say we now try to add a dog into this list, what then happens?? Remember that both dog and cat are subtypes of animal
}
