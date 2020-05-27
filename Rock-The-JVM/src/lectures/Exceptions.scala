package lectures

object Exceptions extends App {

//  throw new OutOfMemoryError("Memory exhausted")
//  throw new StackOverflowError("Stack don overflow")

  case class OverFlowException(message: Option[String] = Option(""))
    extends Exception{
    override def toString: String = s"${super.toString}: ${message.get}"
  }
  case class UnderFlowException(message: Option[String] = Option(""))
    extends Exception{
    override def toString: String = s"${super.toString}: ${message.get}"
  }
  case class MathCalculationException(message: Option[String] = Option("Exception occurred due to division by zero"))
    extends Exception{
    override def toString: String = s"${super.toString}: ${message.get}"
  }

  case object PocketCalculator{

    def add(x: Int, y: Int): Int = {
      lazy val res = x.toDouble + y.toDouble
      if (res > Int.MaxValue.toDouble)
        throw OverFlowException(Option("Maximum value for int exceeded"))
      else res.toInt
    }

    def subtract(x: Int, y: Int): Int = {
      lazy val res = x.toDouble - y.toDouble
      if (res.toDouble < Int.MinValue.toDouble)
        throw UnderFlowException(Option("Minimum value for int exceeded"))
      else res.toInt
    }

    def multiply(x: Int, y: Int): Int = {
      lazy val res = x.toDouble * y.toDouble
      if (res.toDouble > Int.MaxValue.toDouble)
        throw OverFlowException(Option("Maximum value for int exceeded"))
      else if (res.toDouble < Int.MinValue.toDouble)
        throw UnderFlowException(Option("Minimum value for int exceeded"))
      else res.toInt
    }

    def divide(x: Int, y: Int): Int = {
      lazy val res = x / y
      try {
        res
      }catch {
        case _: Throwable => throw MathCalculationException()
      }
    }
  }

  import PocketCalculator._

//  println(add(1, Int.MaxValue))
//  println(subtract(Int.MinValue, 1))
//  println(multiply(2, Int.MaxValue))
//  println(multiply(2, Int.MinValue))
//  println(divide(2, 0))
}
