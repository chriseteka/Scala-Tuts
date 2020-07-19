package lectures.part2AFP

/**
  * PS: EVERY MONAD HAS TWO METHODS:
  *   1. A `flatMap[B](func: A => B)` in the monad's trait.
  *   2. An `apply[A](a: => A)` in the monad's companion object.
  *
  * PS: MONAD MUST OBEY THE FOLLOWING LAWS:
  *   1. left-identity  ===> `unit.flatMap(f) = f(x)`
  *   2. right-identity ===> `f(x).flatMap(unit) = f(x)`
  *   3. associativity  ===> `a.flatMap(f).flatMap(g) = a.flatMap(m => f(m).flatMap(g))`
  */
object Monads extends App{

  //Our own try monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }
  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A]{
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing]{
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  val attempt = Attempt {
    throw new RuntimeException("On purpose fail of my monad")
  }
  println(attempt)
  //Try or Attempt is an abstraction for computations that may fail or not
  //Options are abstraction for computations that may return a value or not

  /*
    EXERCISE 1: Implement a Lazy[T] Monad
    Lazy[T] monad is an abstraction for computations which will only be evaluated when its needed

    EXERCISE 2: Regular Monad = Unit + flatMap
    Implement Monad = Unit + Map + flatten, so given a monad with:
      def flatMap[B](func: T => Monad[B]): Monad[B] = ... (implemented), we want the ffg:
    def map[B](func: T => B): Monad[B] = ???
    def flatten(m: Monad[Monad[T]]): Monad[T] = ???
   */
  trait LazyMonad[+T] {
    def flatMap[A](func: T => LazyMonad[A]): LazyMonad[A]
  }
  object LazyMonad {
    def apply[T](a: => T): LazyMonad[T] = Eval(a)
  }

  case class Eval[+T](value: T) extends LazyMonad[T] {
    override def flatMap[B](func: T => LazyMonad[B]): LazyMonad[B] = func(value)
  }

  val lazyCompute = LazyMonad{
    "My Name is Chris"
  }
  println(lazyCompute)

  //CORRECTION 1
  class Lazy[+A](value: => A) {
    private lazy val internalValue = value //Call by need
    def use: A = internalValue
    def flatMap[B](func: (=> A) => Lazy[B]): Lazy[B] = func(internalValue)
  }
  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("Today I don't feel like doing anything.")
    42
  }

  val flatMappedInstance = lazyInstance.flatMap(x => Lazy {
    10 * x
  })

  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })

  flatMappedInstance.use
  flatMappedInstance2.use

  //CORRECTION 2
  //Given the lazy monad impl like the one above, then we can implement map and flatten as thus
  class Lazy2[+A](value: => A) {
    private lazy val internalValue = value //Call by need
    def use: A = internalValue
    def flatMap[B](func: (=> A) => Lazy2[B]): Lazy2[B] = func(internalValue)
    def map[B](func: (=> A) => B): Lazy2[B] = flatMap(x => Lazy2(func(x)))
    def flatten[B >: A](monad: Lazy2[Lazy2[B]]): Lazy2[B] = monad.flatMap(x => x)
  }
  object Lazy2 {
    def apply[A](value: => A): Lazy2[A] = new Lazy2(value)
  }

  val lazyInts = Lazy2(Lazy2(3))
}
