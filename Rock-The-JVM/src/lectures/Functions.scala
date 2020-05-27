package lectures

import scala.annotation.tailrec

object Functions extends App {

  println("Functions time")

  def aFunction(a: String, b: Int): String =
    a + " " + b

  def aRepeatedFunction(a: String, n: Int): String = {
    if (n == 1) a
    else a + " " + aRepeatedFunction(a, n - 1)
  }
  println(aRepeatedFunction("chris", 3))

  def aFunctionWithSideEffect(a: String): Unit = {
    println(a)
  }
  aFunctionWithSideEffect("Eteka")

  def aBigFunction(i: Int): Int = {
    def aSmallerFunction(a: Int, b: Int): Int = a + b
    aSmallerFunction(i, i - 1)
  }

  println(aBigFunction(2))
  //WHEN YOU NEED LOOPS USE RECURSION

  def factorial(n: Int): Int = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }
  println(factorial(5))

  def fibonacci(n: Int): Int = {
    if (n <= 2) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }
//  println(fibonacci(100))

  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else n % t != 0 && isPrimeUntil(t - 1)
    isPrimeUntil(n / 2)
  }
//  println(isPrime(10))

  def anotherFactorial(n: Int): BigInt = {
    @tailrec
    def factorialHelper(x: Int, accumulator: BigInt = 1): BigInt = {
      if (x <= 1) accumulator
      else factorialHelper(x - 1, x * accumulator)
    }
    factorialHelper(n)
  }
//  println(anotherFactorial(20000))

  def anotherFibonacci(n: Int): BigInt = {
    @tailrec
    def fibonacciHelper(x: Int = 2, last: BigInt = 1, nextLast: BigInt = 1): BigInt = {
      if (x >= n) last
      else fibonacciHelper(x + 1, last + nextLast, last)
    }

    if (n <= 2) 1
    else fibonacciHelper()
  }
//  println(anotherFibonacci(1000))

  def strConcat(s: String, n: Int): String = {
    @tailrec
    def concat(y: Int, res: String = " ") : String = {
      if (y <= 0) res
      else concat(y - 1, s"$s $res")
    }
    concat(n)
  }
//  println(strConcat("Eteka", 3))

  def anotherIsPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeUntil(x: Int = n / 2, isStillPrime: Boolean = true): Boolean = {
      if (!isStillPrime) false
      else if (x <= 1) true
      else isPrimeUntil(x - 1, n % x != 0 && isStillPrime)
    }
    isPrimeUntil()
  }

  println(anotherIsPrime(2003))
}
