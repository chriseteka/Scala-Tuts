package exercises

import scala.annotation.tailrec


/*
  EXERCISES
    Implement a lazily evaluated singly linked STREAM of elements.
  Stream is a special kind of collection in that its head is always available but its
  tail is only evaluated on demand.
 */
abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] //Prepend elements to Stream
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] //Concatenate streams

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  //Takes the first n elements in a stream, and returns it as another stream
  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
}

object EmptyStream extends MyStream[Nothing] {

  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyStream[Nothing] = throw new NoSuchElementException

  override def #::[B >: Nothing](element: B): MyStream[B] = new NoneEmptyStream(element, this)

  override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  override def foreach(f: Nothing => Unit): Unit = ()

  override def map[B](f: Nothing => B): MyStream[B] = this

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = this
}

class NoneEmptyStream[+A](val hd: A, tl: => MyStream[A]) extends MyStream[A]{

  override def isEmpty: Boolean = false

  //Observe we have changed head and tail to val and not a def,
  //hence they will be evaluated only once and then used as many times within this class.
  override val head: A = hd

  //We call by need, cause we have combined lazy val and call by name from the class parameter
  override lazy val tail: MyStream[A] = tl

  override def #::[B >: A](element: B): MyStream[B] = new NoneEmptyStream[B](element, this)

  override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new NoneEmptyStream[B](head, tail ++ anotherStream)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def map[B](f: A => B): MyStream[B] = new NoneEmptyStream[B](f(head), tail map f)

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MyStream[A] = {
    if (predicate(head)) new NoneEmptyStream[A](head, tail filter predicate)
    else tail filter predicate
  }

  override def take(n: Int): MyStream[A] = {
    if (n == 0) EmptyStream
    else if (n == 1) new NoneEmptyStream[A](head, EmptyStream)
    else new NoneEmptyStream[A](head, tail.take(n - 1))
  }
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new NoneEmptyStream[A](start, MyStream.from(generator(start))(generator))
}

object MyStreamTrial extends App {

  val naturalNumbers = MyStream.from(1)(_ + 1)
//  println(naturalNumbers.head) // I expect to get 1 here
//  println(naturalNumbers.tail.head) // I expect to get 2 here
//  println(naturalNumbers.tail.tail.head) // I expect to get 3 here

  val startFrom0 = 0 #:: naturalNumbers //equiv to naturalNumbers.#::(0) prepending
//  println(startFrom0.head)

//  startFrom0.take(100000).foreach(println)
//  startFrom0.map(_ * 2).takeAsList(100).foreach(println)
  println(startFrom0.flatMap(x => new NoneEmptyStream(x, new NoneEmptyStream(x + 1, EmptyStream))).takeAsList(10))
//  println(startFrom0.filter(_ < 10).takeAsList(11)) //Impossible to find hence error
//  println(startFrom0.filter(_ > 10).takeAsList(10))
//  println(startFrom0.filter(_ > 10).take(10).take(20).toList()) //Will return the 10 initially gotten

  //EXERCISE:
  //1. Generate the stream of fibonacci numbers
  /*
    FIBONACCI NUMBERS TAKE THE FORM: 1, 1, 2, 3, 5, 8, 13, 21, ...
      Formed from the principle: right-most number + immediate left = next number
   */
  //2. Generate the stream of prime numbers with Eratosthenes' sieve
  /*
    ERATOSTHENES' SIEVE: Simply generates n-infinite prime numbers with the pattern:
      Given: [2, 3, 4, 5, 6, 7, ...] ==> A set of natural numbers starting from 2
        then step 1: Sieve out all numbers divisible by 2, but keep 2. [2, 3, 5, 7, 9, ...]
        then step 2: Sieve out all numbers divisible by 3, but keep 3. [2, 3, 5, 7, 11, ...]
          .
            .
              .
        Same pattern downwards, this will result in an infinite prime numbers starting with 2.
   */

  //EXERCISE 1:
  val fibonacciGenerator: Int => BigInt = { n =>
    @tailrec
    def fibonacciHelper(x: Int = 2, last: BigInt = 1, nextLast: BigInt = 1): BigInt = {
      if (x >= n) last
      else fibonacciHelper(x + 1, last + nextLast, last)
    }
    fibonacciHelper()
  }
  val fibonacciStream: MyStream[BigInt] = naturalNumbers.map(fibonacciGenerator)
  println(fibonacciStream.takeAsList(10))

  //EXERCISE 2:
  val eratosthenesSieve: Int => Boolean = { n =>
    @tailrec
    def isPrimeUntil(x: Int = n / 2, isStillPrime: Boolean = true): Boolean = {
      if (!isStillPrime) false
      else if (x <= 1) true
      else isPrimeUntil(x - 1, n % x != 0 && isStillPrime)
    }
    isPrimeUntil()
  }
  val eratosthenesStream = naturalNumbers.filter(num => num > 1 && eratosthenesSieve(num))
  println(eratosthenesStream.takeAsList(10))

  //EXERCISE 1 CORRECTION
  def fibonacciStream_2(first: BigInt = 1, next: BigInt = 1): MyStream[BigInt] =
    new NoneEmptyStream[BigInt](first, fibonacciStream_2(next, first + next))
  println(fibonacciStream_2().takeAsList(100))

  //EXERCISE 2 CORRECTION
  def eratosthenesSieve_2(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else new NoneEmptyStream[Int](numbers.head, eratosthenesSieve_2(numbers.filter(_ % numbers.head != 0)))
  println(eratosthenesSieve_2(MyStream.from(2)(_ + 1)).takeAsList(100))
}
