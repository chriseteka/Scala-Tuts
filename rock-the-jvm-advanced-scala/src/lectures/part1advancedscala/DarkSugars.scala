package lectures.part1advancedscala

//Syntax sugar
object DarkSugars  extends App {

  //#1: Methods with a single param
  def singleArgMethod(x: Int) = s"$x little ducks..."

  val description = singleArgMethod {
    //Write codes
    42
  }

  //This explains why we can do
  List(1,2,3).map { x =>
    x + 1
  }

  //#2: Single abstract method, when a trait has just 1 operation, it can be reduced to lamda
  trait Action{
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  //The above can be re-written as
  val aFunkyInstance: Action = (x: Int) => x + 1

  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello Thread")
  })

  val sweetThread = new Thread(() => println("Sweet Scala"))

  //This style also works for an abstract class which has many methods
  // implemented but has only one unimplemented method, observe below
  abstract class anAbstractType {
    def implemented: Int = 23
    def abstractMethod(a: Int): Unit
  }

  val anAbstractinstance: anAbstractType = (_: Int) => println("Sweet")


  //#3 the :: and #:: methods are special
  //This happens because of the scala spec which says that the last character decides
  //the associativity of a method whenever : (colon) is used, hence the operation evaluates
  // by starting from the List side first, and yields thus: List(3, 4).::(2)
  val prePendedList = 2 :: List(3, 4)

  class MyList[T] {
    def -->:(value: T): MyList[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyList[Int]
  println(myStream)

  //#4: Multi word method naming
  class TeenGirl(name: String){
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lily = new TeenGirl("Lily")
  lily `and then said` "Scala is so sweet!"

  //#5: infix types
  class Composite[A, B]
  val composite: Composite[Int, String] = ???
  //The above can be re-written thus:
  val composite2: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???

  //#6: Update() method: Special just like the apply()
  val anArray = Array(1, 2, 3)
  anArray(2) = 7
  //The compiler writes it as anArray.update(2, 7) => used in mutable collections
  //Hence just like we use apply to call objects like it were functions, we can define
  //updates and use them in the fashion above, remember it takes 2 args,
  //these are: 1-> the index to update, 2-> The new value to update this index with

  //#7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0

    def member = internalMember //getter
    //Observe how we have used the underscore and equals sign with the braces
    def member_=(value: Int) = internalMember = value //Setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 //This is re-written as aMutableContainer.member_=(42)

}
