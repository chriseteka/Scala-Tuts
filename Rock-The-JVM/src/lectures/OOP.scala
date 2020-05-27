package lectures

import java.time._
import java.time.temporal.ChronoUnit

object OOP extends App {

  val authorChris = Writer("Christopher", "Eteka", Option(LocalDate.of(1997, 6, 1)))
  val authorArinze = Writer("Arinze", "Ogbonna")

  println("Printing authors registered to the system")
  println(authorChris)
  println(authorArinze)
  println("*********************************************")
  println("Printing authors full Names")
  println(authorChris.fullName())
  println(authorArinze.fullName())
  println("*********************************************")

  val chrisNovel = Novel("Scala Junky", Option(LocalDate.of(2018, 4, 16)), Option(authorChris))
  val arinzeNovel = Novel("Js sucks", author = Option(authorArinze))

  println("Printing novels registered to the system")
  println(chrisNovel)
  println(arinzeNovel)
  println("*********************************************")
  println("Printing authors age from the novel that are registered")
  println(chrisNovel.authorAge())
  println(arinzeNovel.authorAge())
  println("*********************************************")
  println("Printing TRUE or FALSE if the author of a book is actually the author passed")
  println(chrisNovel.isWrittenBy(authorArinze))
  println(chrisNovel.isWrittenBy(authorChris))
  println(arinzeNovel.isWrittenBy(authorChris))
  println(arinzeNovel.isWrittenBy(authorArinze))
  println(arinzeNovel.isWrittenBy(null))
  println("*********************************************")
  println("Adding a released year to Arinze's novel and printing it out")
  println(arinzeNovel.copy(LocalDate.of(2016, 7, 16)))
  println("Checking if the newly returned books author is same as the old, after updating released year.")
  println(arinzeNovel.isWrittenBy(arinzeNovel.copy(LocalDate.of(2016, 7, 16)).author.orNull))
  println("*********************************************")

  val counterOneHundred = Counter(100)

  println("Now working with the counter shit")
  println(counterOneHundred.currCount)
  println(counterOneHundred.incCounter)
  println(counterOneHundred.incCounter(100))
  println(counterOneHundred.decrCounter)
  println(counterOneHundred.decrCounter(100))
}

case class Writer(fName: String, sName: String, var dob: Option[LocalDate] = None){

  //Other methods
  def fullName(): String = s"$fName $sName"

  override def equals(obj: Any): Boolean = {
    if (obj == null) return false
    val incomingA = obj.asInstanceOf[Writer]
    incomingA.fullName().equalsIgnoreCase(this.fullName())&&
      (incomingA.dob.exists(_.isEqual(this.dob.orNull)) || (incomingA.dob.isEmpty && this.dob.isEmpty))
  }

  override def toString: String = s"Writer[fName=$fName, sName=$sName, dob=${dob.orNull}]"
}

case class Novel(name: String, releaseYear: Option[LocalDate] = None,
                 author: Option[Writer] = None){

  //Other methods
  def authorAge(): Long = author.map(
    _.dob.map(
      _.until(LocalDate.now(), ChronoUnit.YEARS)
    ).getOrElse(0L)
  ).getOrElse(0L)

  def isWrittenBy(author: Writer): Boolean = this.author.exists(_.equals(author))

  def copy(releasedYear: LocalDate): Novel = Novel(name, Option(releasedYear), author)

  override def toString: String = s"Novel[name=$name, releasedYear=${releaseYear.orNull}, author=${author.orNull}]"
}

case class Counter(value: Int){

  //Other methods
  def currCount: Int = value
  def incCounter: Counter = Counter(value + 1)
  def incCounter(incBy: Int): Counter = {
    if (incBy <= 0) this
    else incCounter.incCounter(incBy - 1)
  }
  def decrCounter: Counter = Counter(value - 1)
  def decrCounter(decBy: Int): Counter = {
    if (decBy <= 0) this
    else decrCounter.decrCounter(decBy - 1)
  }

  override def toString: String = s"Counter[value=$value]"
}
