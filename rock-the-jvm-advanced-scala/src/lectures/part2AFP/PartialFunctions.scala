package lectures.part2AFP

import java.util.Scanner

import scala.io.Source

object PartialFunctions extends App {

  //Remember. The function below accepts any Int and returns an Int
  val aFunction = (x: Int) => x + 1 //Function1[Int, Int] === Int => Int
  //We can however declare a function to accept some kind of values
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicable

  class FunctionNotApplicable extends RuntimeException

  //We can modify the above to use pattern matching as thus:
  //We are guaranteed from the function below that if no match is found, the a Mismatch
  //error is thrown
  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  //The above is a ways to restrict our function to some certain Int but not in an elegant
  //manner, which is where the partial function come into play.
  //Notice that the pattern match only accept {1, 2, 5} which are in the domain of Ints
  //We can say that only some part in the domain of Ints {1,2,5} => Int are acceptable
  //This is what is referred to as partial function (Parts of a domain) are handled.
  //In scala, this is best written thus:
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } //Partial function values. This allows us call the function as we normally do
  println(aPartialFunction(2))
  //If we try to call a number which is not supported, then the call will crash with a
  //match error, this is because partial function is based on pattern matching.
//  println(aPartialFunction(10))

  //PF Utilities
  //1. Check if a partial function is applicable for a given value
  println(aPartialFunction.isDefinedAt(10))
  //2. Lifting of partial function: making a PF become a total function returning Options
  //Rem: Options allow the use of Some(supportedValueFound) and/or None, when val is unsupported
  val lifted = aPartialFunction.lift //Turns our PF to a function from Int => Option[Int]
  println(lifted(2)) //Returns Some(56)
  println(lifted(21)) //Returns None
  //The above util is useful for allowing us call a PF with unsupported value and yet
  //Not crash our program, whilst returning us a useful result
  //3. Chaining of PF with the use of orElse[From, To], allows us specify more cases
  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  //The above allows us to call both supported values from the initially defined PF and
  //the current value also defined in itself, observe the outputs below:
  println(pfChain(2))
  println(pfChain(45))

  //NB: PF extends normal functions, hence a normal function can be written like a PF, since
  //PF is a child, and has implementations to support the declaration, the function would be
  //considered to be a PF subtype of a normal function. An instance is below
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }
  // As an advantage, HOFs accepts PFs, observe below
  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 47
    case 3 =>  1000
  } //Notice this is a PF, which is immediately declared in a map function
  println(aMappedList)

  //NB: unlike functions that have many param types, PF can only have ONE param type
  val aFunc = (a: String, b: Int) =>  s"$a, $b" //equiv (String, Int) => String
  //The above cannot be converted to a PF

  /**
    * EXERCISE
    * 1. Construct a PF instance by instantiating the PF trait on the spot (Anonymous class)
    * 2. Implement a small Dumb chat bot as a PF
    */
  val anonPF = new PartialFunction[Int, Int] {
    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2 || x == 3

    override def apply(v1: Int): Int = v1 match {
      case 1 => 11
      case 2 => 22
      case 3 => 33
    }
  }
  println(anonPF.isDefinedAt(9))
  println(anonPF(3))

  val bot = "AssistantDru"
  val botReplies: String => String = {
    case "Hello" => s"$bot: Hello, how can I help you?"
    case _ => s"$bot: I cannot help you with this at the moment" +
      s"\n$bot: Enter another message >:"
  }

  val input = new Scanner(System.in)
  println(s"$bot: Hi, am Assistant Dru...")
  print(s"$bot: Enter a message here, and I will reply >: ")
//  Source.stdin.getLines().foreach(line => println(botReplies(line)))
  Source.stdin.getLines().map(botReplies).foreach(println)
}
