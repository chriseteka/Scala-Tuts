package lectures.part1advancedscala

object AdvancedPatternMatching  extends App {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"The only element is $head")
    case _ =>
  }

  /*
    Note that we try to match against:
    - Constants
    - Wildcards
    - Case classes
    - Tuples
    - Some special magic like we did above
   */

  //Below is how we make a class compatible with pattern matching
  //NB: By default case classes are compatible with pattern matching
  class Person(val name: String, val age: Int)
  //1. Define a companion object, this is not compulsory, we can name it anything.
  object Person {

    //2. Define a method called unapply, that takes an arg of type Person, (Compulsory)
    //that returns an option of the type you wish to decompose, so if you want a name,
    //then the return type will be Option[String]. If you wan the name and age,
    //then the return type will be Option[(String, Int)]
    def unapply(arg: Person): Option[(String, Int)] = Some((arg.name, arg.age))
    //Observe that the unapply method can be overloaded, observe below
    def unapply(age: Int): Option[String] = Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 20)
  val greeting = bob match {
    case Person(name, age) => s"Hi, my name is $name, and I am $age years old."
  }

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(greeting)
  println(legalStatus)

  /*
    EXERCISE: We don't want the kind of pattern match stated below, we want an elegant
    or improved pattern matching style for something of such example.
   */
//  val n: Int = 45
//  val mathProperty = n match {
//    case x if x < 10 => "Single Digit"
//    case x if x % 2 == 0 => "Even Numbers"
//  }

  //My Approach
  //Define an unapply method which returns me a list of all the xteristics of the num
  //If i pattern match against the said number, I should get a list of its characteristics

  val singleDigitProperty = (x: Int) => if (x < 10) "Single Digit" else "Double Digit"
  val evenNumberProperty = (x: Int) => if (x % 2 == 0) "Even Number" else "Odd Number"
  val positiveNumberProperty = (x: Int) => if (x > 0) "Positive Number" else "Negative Number"

  object NumPropsDeterminer {
    def unapply(arg: Int): Option[List[String]] =
      Some(List(singleDigitProperty(arg), evenNumberProperty(arg), positiveNumberProperty(arg)))
  }

  val checkProp: List[String] = -17 match {
    case NumPropsDeterminer(props) => props
  }
  checkProp.foreach(println)

  //Correction
  object singleDigit {
    def unapply(arg: Int): Option[Boolean] =
      if (arg > -10 && arg < 10) Some(true)
      else None
  }

  object even {
    def unapply(arg: Int): Option[Boolean] =
      if (arg % 2 == 0) Some(true)
      else None
  }

//  val n: Int = 45
//  val mathProperty = n match {
//    case singleDigit(_) => "Single Digit"
//    case even(_) => "Even Numbers"
//    case _ => "No property"
//  }
//
//  println(mathProperty)

  //We can further enhance the above correction and make the unapply return a boolean
  object singleDigitAdv {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  object evenAdv {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  val n: Int = 45
  val mathProperty = n match {
    case singleDigitAdv() => "Single Digit"
    case evenAdv() => "Even Numbers"
    case _ => "No property"
  }

  println(mathProperty)

  //Infix patterns
  //NB: To achieve infix pattern, you have to declare your cas class thus:
  case class Or[A, B](a: A, b: B) //This type is known as Either, similar to Option
  val either = Or(2, "two")
  val humanDescription = either match {
//    case Or(number, str) => s"$number is written as $str" //Normal way of matching a class
    //The above can however be re-written thus
    //This is more readable and referred to as infix pattern
    case number Or str => s"$number is written as $str"
  }
  println(humanDescription)

  //Decomposing sequences, nb: that the vararg pattern is written as _*
  val vararg = numbers match {
    // This means pattern matching against the entire list as a sequence
    // This list may have 1 or more values that can be decomposed,
    // hence the standard technique for un applying for a list does not work here,
    // for this we need our own unapply method which is defined below
    case List(1, _*) => "Starting with one"
  }

  abstract class MyList[+A]{
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "Starting with 1 and 2"
    case _ => "Something else"
  }
  println(decomposed)

  //Custom return type for unapply method. The return type for unapply or unapply sequence
  // must not necessary be an option, It can be a class that has a boolean function and a get
  //The data structure that must be used must have:
  //1. isEmpty: Boolean
  //2. get: Something
  //Example below

  abstract class Wrapper[T]{
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false

      override def get: String = person.name
    }
  }

  println(bob match {
    case PersonWrapper(name) => s"This person's name is $name"
    case _ => "An alien"
  })
}
