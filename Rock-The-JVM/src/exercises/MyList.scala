package exercises

abstract class MyList[+T] {

  def head: T
  def tail: MyList[T]
  def isEmpty: Boolean
  def add[U >: T](i: U): MyList[U]
  def printElements: String
  //Polymorphic call
  override def toString: String = s"[ $printElements ]"

  def map[B](transformer: MyTransformer[T, B]): MyList[B]
  def flatMap[B](transformer: MyTransformer[T, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[T]): MyList[T]

  //Concatenation
  def ++[U >: T](list: MyList[U]): MyList[U]
}

trait MyPredicate[-T]{
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B]{
  def transform(elem: A): B
}

case object Empty extends MyList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add[A >: Nothing](i: A): MyList[A] = Cons[A](i, Empty)

  override def printElements: String = ""

  override def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty

  override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty

  override def ++[U >: Nothing](list: MyList[U]): MyList[U] = list
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {

  override def head: A = h

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def add[U >: A](i: U): MyList[U] = Cons(i, this)

  override def printElements: String = {
    if (tail.isEmpty) s"$h"
    else s"$h ${tail.printElements}"
  }

  override def map[B](transformer: MyTransformer[A, B]): MyList[B] =
    Cons(transformer.transform(h), t.map(transformer))

  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)

  override def filter(predicate: MyPredicate[A]): MyList[A] = {
    if (predicate.test(h)) Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }

  override def ++[U >: A](list: MyList[U]): MyList[U] = Cons(h, t ++ list)
}

case class Person(name:String, age: Option[Int] = Option(0))

object ListTest extends App {

  //List of Int Created from the generic methods defined above
  val intList = Cons(1, Cons(2, Cons(3, Empty)))
  val anotherIntList = Cons(4, Cons(5, Empty))
  println(intList.tail.head)
  println(intList.add(4).head)
  println(intList.toString)
//  println(intList.map((elem: Int) => elem * 2))
  println(intList.map(new MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }).toString)
//  println(intList.filter((elem: Int) => elem % 2 == 0).toString)
  println(intList.filter(new MyPredicate[Int] {
    override def test(elem: Int): Boolean = elem % 2 == 0
  }).toString)
  println((intList ++ anotherIntList).toString)
//  println(intList.flatMap((elem: Int) => Cons(elem, Cons(elem + 1, Empty))).toString)
  println(intList.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(elem: Int): MyList[Int] = Cons(elem, Cons(elem + 1, Empty))
  }).toString)

  //List of String Created from the generic method defined above
  val stringList = Cons("Chris", Cons("Arinze", Cons("Osita", Empty)))
  println(stringList.tail.head)
  println(stringList.add("Charlie").head)
  println(stringList.toString)

  //List of Person Created from the generic method defined above
  val personList = Cons(Person("Chris"), Cons(Person("Arinze"), Cons(Person("Osita"), Empty)))
  println(personList.tail.head)
  println(personList.add(Person("Charlie")).head)
  println(stringList.toString)

  class EvenPredicate() extends MyPredicate[Int] {
    override def test(elem: Int): Boolean = elem % 2 == 0
  }

  class StringToIntTransformer extends MyTransformer[String, Int] {
    override def transform(elem: String): Int = elem.toInt
  }

  class MultiplyBy2 extends MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }

  object Testers {
    def isEven(i: Int): Boolean = new EvenPredicate().test(i)
    def strToInt(i: String): Int = new StringToIntTransformer().transform(i)
    def multiplier(i: Int): Int = new MultiplyBy2().transform(i)
  }

  println(Testers.isEven(8))
  println(Testers.strToInt("8"))
}
