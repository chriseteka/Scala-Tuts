package lectures

object CallBy extends App {

  //The exact value for the function is evaluated once before the function is called
  def callByValue(x: Long): Unit = {
    //x will have same value when the function is called
    println("Method called by value: " + x)
    println("Method called by value: " + x)
  }

  //The exact value for the function is evaluated every time, when needed
  def callByName(x: => Long): Unit = {
    //x is re-evaluated each time it is called upon
    println("Method called by value: " +  x)
    println("Method called by value: " +  x)
  }

  callByValue(System.nanoTime())
  callByName(System.nanoTime())

  //The by-name param call is a lazy way of calling a param, hence, only when the param
  //is needed will it be evaluated, else, it is never evaluated, this saves us memory
  // and potential error in a case where the param would have resulted to an error but
  // if it is never called, its computation will never happen, take for example:
  def infinite(): Int = 1 + infinite()
  def evaluateFirst(x: Int, y: => Int): Unit = println(s"First data is: $x")

  //This throws error, since infinite will have to be computed and is a function that runs
  //to infinity, hence will never be computed successfully
//  evaluateFirst(infinite(), 20)

  //This wont throw an error since 20 (the first number) will be printed and hence no need
  //to start computing infinite
  evaluateFirst(20, infinite())

  println(s"crazy thing i just thought of: ${def justGiveAString(): String = "Awesome right."; justGiveAString()}, javascript shit")
}
