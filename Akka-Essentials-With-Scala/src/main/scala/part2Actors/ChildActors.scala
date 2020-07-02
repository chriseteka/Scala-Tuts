package part2Actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2Actors.ChildActors.CreditCard.{AttachToAccount, CheckStatus}

object ChildActors extends App {

  //Actors can create other actors. Allows us to create actor hierarchies
  // The example shows how a parent creates a child and can further create other children

  object Parent {
    case class CreateChild(name: String)
    case class TellChild(message: String)
  }

  class Parent extends Actor {
    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"[${self.path}] creating child")
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(child: ActorRef): Receive = {
      case TellChild(message) => child forward message
    }
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message: String => println(s"[${self.path}] I got: $message")
    }
  }

  import Parent._

  val system = ActorSystem("ParentChildDemo")
  val parent = system.actorOf(Props[Parent], "parent")
  parent ! CreateChild("child")
  parent ! TellChild("Hey Kid")

  //WHo now owns a parent?
  /*
    GUARDIAN ACTORS (TOP LEVEL ACTORS) they are 3:
    1. /system = system guardian (oversees all actors in different sections, e.g user sec, login sec)
    2. /user = user level guardian (oversees actors we create using the actor system)
    3. / = the root guardian (oversees all actors, it is the base of every actor)
   */

  //Lets now consider a feature which akka supports to find an actor by path
  /*
    This techniques is called the ACTOR SELECTION, below is how it works
    NB: It only returns a possible actor ref, just like how Option[] works,
    In a case where an actor is not found and you try to send a message to it, this won't work
   */
  val childSelector = system.actorSelection("/user/parent/child")
  childSelector ! "I found you"

  /**
    * Danger
    *
    * NEVER PASS MUTABLE ACTOR STATE, OR THE `THIS` REFERENCE, TO CHILD ACTORS.
    *
    * NEVER IN YOUR LIFE, IT MAY BREAK ACTOR ENCAPSULATION, lets demo with an example
    */

  object NaiveBankAccount {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object InitializeAccount
  }
  class NaiveBankAccount extends Actor {
    import NaiveBankAccount._
    import CreditCard._

    var amount = 0
    override def receive: Receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "card")
        creditCardRef ! AttachToAccount(this) // This makes matter worse when we pass the `this` ref
      case Deposit(funds) => depositFunds(funds)
      case Withdraw(funds) => withdrawFunds(funds)
    }

    def depositFunds(funds: Int): Unit = {
      println(s"${self.path} depositing $funds on top of $amount")
      amount += funds
    }
    def withdrawFunds(funds: Int): Unit = {
      println(s"${self.path} withdrawing $funds from $amount")
      amount -= funds
    }
  }

  object  CreditCard {
    case class AttachToAccount(bankAccount: NaiveBankAccount) // This is so very wrong, this is supposed to take a ref
//    case class AttachToAccount(bankAccount: ActorRef) // This must be how to define ref to an actor
    case object CheckStatus
  }
  class CreditCard extends Actor {
    override def receive: Receive = {
      case AttachToAccount(account) =>
        context.become(attachTo(account))
    }

    def attachTo(account: NaiveBankAccount): Receive = {
      case CheckStatus =>
        println(s"${self.path} your message have been processed")
        account.withdrawFunds(1) // This is extremely wrong and extremely hard to debug
      /**
        * This bypasses all security of the actor, since you are trying to access it like an object
        * The problem originated when we defined AttachAccount() to take a param of type NaiveBankAccount
        * We have stated times without number that you simply pass the ref to what ever class that extends an actor
        * And not the class itself, then to communicate with it, you use the message-like protocol
        *
        * The big problem this will pose is concurrency issue, which is what actors has been trying to solve
        * You never ever call methods in an actor, you communicate with them through messages and let them do their thing
        * When you do a thing like this, an actor is exposed and concurrency is lost
        *
        * What we just did is referred to as `CLOSING OVER`, as a warning:
        * NEVER CLOSE OVER MUTABLE STATE (classes that extends actor) or `this`
        */
    }
  }

  import NaiveBankAccount._
  import CreditCard._

  val bankAccountRef = system.actorOf(Props[NaiveBankAccount], "account")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(100)


  Thread.sleep(500)
  val creditCardSelection = system.actorSelection("/user/account/card")
  creditCardSelection ! CheckStatus
}
