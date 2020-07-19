package lectures.part2AFP

object CurriesAndPAF extends App {

  //REM: Curried function
  val supperAdder: Int => Int => Int = x => y => x + y
  val add3 = supperAdder(3)
  println(s"Total: ${add3(5)}")
  println(s"Total: ${supperAdder(5)(10)}")
  //supperAdder is a curried function, because it takes multiple parameter lists

  //This is a curried method like the one above
  def curriedAdder(x: Int)(y: Int): Int = x + y
  //To call this function with a single param, you however must specify the return type
  //of the variable where this function must be stored into.
  //val adder4 = curriedAdder(4) //Wont work, will throw an exception
  val adder4: Int => Int = curriedAdder(4) //The right way
  //We can achieve same thing by leveraging on the ETA-expansion done behind the scene
  val add5 = curriedAdder(5)_

  println(add5(4))

  //EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int): Int = x + y
  def curriedAddMethod(x: Int)(y: Int): Int = x + y

  //We want to define add7 based off of the three definitions above.
  val add7SAF = simpleAddFunction(_, 7)
  println(add7SAF(100))
  val add7SAM = simpleAddMethod(7, _)
  println(add7SAM(200))
  val add7CAM = curriedAddMethod(7)_
  println(add7CAM(300))

  //This solution works for all man
  val add7 = (x: Int) => simpleAddFunction(7, x)
  println(add7(213))
  //Converts a function or method to its curried form,
  // doesn't work for an already curried function
  val add7_2 = simpleAddFunction.curried(7)
  println(add7_2(213))
  val add7_3 = curriedAddMethod(7)(_)
  println(add7_3(113))


  //EXERCISES
  /*
    process a list of numbers and return their string representations with different formats
    Use the %4.2f, %8.6f, %14.12f with a curried function formatter
   */
  //Below is a method declaration for a curry.
  def strFormatter(formatter: String)(num: Double): String =
  formatter.format(num)
  //Below is a function declaration for a curry
  val strFormatter_2: String => Double => String =
    formatter => num => formatter.format(num)
  //Both definitions above are curried and can be called as one,
  // they take a string and an int and return a string, which is formatted.

  //Format 4.2
  val format4_2 = strFormatter("%4.2f")_
  val another_format4_2 = strFormatter_2("%4.2f")(_: Double)

  //Format 8.6
  val format8_6 = strFormatter("%8.6f")_
  val another_format8_6 = strFormatter_2("%8.6f")(_: Double)

  //Format 14.12
  val format14_12 = strFormatter("%14.12f")_
  val another_format14_12 = strFormatter_2("%14.12f")(_: Double)

  val numberList = List(1.2, 7.8, 11.5675434565543324, 18.3236754)

  //Now we output them in different formats
  println(numberList.map(another_format14_12))

  /*
    EXERCISE 2
    WE WANT TO DIFFERENTIATE BETWEEN FUNC AND METHODS
    PARAMS: BY-NAME AND 0-LAMBDA
   */
  def byName(n: => Int): Int = n + 1
  def byFunction(f: () => Int): Int = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 43

  println(parenMethod())
}