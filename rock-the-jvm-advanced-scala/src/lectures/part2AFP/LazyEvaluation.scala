package lectures.part2AFP

import scala.annotation.tailrec

object LazyEvaluation extends App {

  //The keyword lazy delays the evaluation of values, making variables with such keyword
  //only evaluated whenever they are called for the first time.
  lazy val x: Int = {
    println("Hello")
    42
  }

  //On first call, Hello is printed to the console before 42 is assigned to x.
  //On subsequent calls, the value 42 is simply output without hello
  println(x)
  println(x)

  //Implications of lazy evaluations
  //1. Lazy evaluations and side effects
  def sideEffectCondition: Boolean = {
    println("Side Effect")
    true
  }
  def simpleCondition: Boolean = true
  lazy val lazyCondition = sideEffectCondition

  println(if (simpleCondition && lazyCondition) "YES" else "NO")

  //2. Lazy evaluations with by name, consider the ffg:
  def byNameMethod(t: => Int): Int = {
    lazy val n = t //Instead of t to be called 3 times, we pass it to a lazy eval variable
    n + n + n + 1
  } // Observe we call n 3 times on purpose
  def retrieveNValue: Int = {
    //Do some long computation which we rep by thread sleep
    println("Evaluating N")
    Thread.sleep(1000)
    42
  }
  //The above technique is what is referred to as call by need
  println(byNameMethod(retrieveNValue))

  //3. filtering with lazy eval
  def lessThan30(n: Int):Boolean = {
    println(s"$n is less than 30?")
    n < 30
  }

  def greaterThan20(n: Int):Boolean = {
    println(s"$n is greater than 20?")
    n > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20) //Observe from the output that all values are evaluated eagerly

  //This helps to show that the withFilter function helps to evaluate values lazily
  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  println
  println(gt20lazy) //Observe that this does not print out the values in it but its wrapper
  //This prints the value, by executing both filters lazily, as if they were chained
  gt20lazy.foreach(println)

  //4. For comprehension use withFilter with guards
  for {
    a <- List(1, 2, 3) if a % 2 == 0 //The if section forces the use of lazy evaluation
  } yield a + 1
  //The above evaluates to the below lazy evaluated code
  List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1)
  //They both return List[Int], and are both lazily evaluated
}
