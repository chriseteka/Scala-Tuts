package exercises

import scala.annotation.tailrec
import scala.language.postfixOps

//We cleared a trait to extend a function (i.e this trait is a subtype of the func)
trait MySet[A] extends (A => Boolean) {

  //This is a follow come trait which must be override since our class extends a function
  //from type A to Boolean. This method is the implementation to that extension.
  override def apply(x: A): Boolean = contains(x)

  //Below are characteristics in this trait that must be override if any class extends
  //or mixes in this trait.
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](func: A => B): MySet[B]
  def flatMap[B](func: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(func: A => Unit): Unit
  def -(elem: A): MySet[A] //Remove
  def &(set: MySet[A]): MySet[A] //Intersection
  def --(set: MySet[A]): MySet[A] //Difference
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {

  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](func: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](func: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(func: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def &(set: MySet[A]): MySet[A] = this

  override def --(set: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = this
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  override def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  override def map[B](func: A => B): MySet[B] =
    (tail map func) + func(head)

  override def flatMap[B](func: A => MySet[B]): MySet[B] =
    (tail flatMap func) ++ func(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(func: A => Unit): Unit = {
    func(head)
    tail foreach func
  }

  override def -(elem: A): MySet[A] = {
    if (head == elem) tail
    else (tail - elem) + head
  }

//  override def &(set: MySet[A]): MySet[A] = filter(x => set.contains(x))
//  override def &(set: MySet[A]): MySet[A] = filter(set.contains(_))
//  override def &(set: MySet[A]): MySet[A] = filter(set.contains)
//  override def &(set: MySet[A]): MySet[A] = filter(set contains)
//  override def &(set: MySet[A]): MySet[A] = filter(x => set(x))
//  override def &(set: MySet[A]): MySet[A] = filter(set(_))
  override def &(set: MySet[A]): MySet[A] = filter(set)

//  override def --(set: MySet[A]): MySet[A] = filter(x => !set.contains(x))
//  override def --(set: MySet[A]): MySet[A] = filter(x => !set(x))
//  override def --(set: MySet[A]): MySet[A] = filter(!set.contains(_))
//  override def --(set: MySet[A]): MySet[A] = filter(!set(_))
  override def --(set: MySet[A]): MySet[A] = filter(!set)

  override def unary_! : MySet[A] = filter(this.contains)
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], accumulator: MySet[A]): MySet[A] = {
      if (valSeq.isEmpty) accumulator
      else buildSet(valSeq.tail, accumulator + valSeq.head)
    }
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayGround extends App {

  val s1 = MySet(1, 2, 3, 4, 5)
  val s2 = MySet(1, 2, 3, 4, 6, 9)
  s1 -- s2 foreach println
//  s + 5 ++ MySet(-1, -2) + 3 map (x => x * 10) foreach println
//  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, x * 10)) filter (_ % 2 != 0) foreach println
}
