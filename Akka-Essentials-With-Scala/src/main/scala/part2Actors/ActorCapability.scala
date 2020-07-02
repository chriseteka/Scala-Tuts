package part2Actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapability extends App {

  val system = ActorSystem("actorsCapabilitiesDemo")

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => context.sender() ! "Hello, there!" //Replying to a message
      case message: String => println(s"[${self.path}] I have received: $message")
      case number: Int => println(s"[Simple Actor] I have received a number: $number")
      case SpecialMessage(contents) => println(s"[Simple Actor] I have received something special $contents")
      case SendMessageToYourself(content) => self ! content
      case SayHiTo(ref) => ref ! "Hi!" // Although Bob says Hi to himself, the sender is still alice
        //With forward in play, sender is changed to dead letters and the receiver is "ref"
      case WirelessPhoneMessage(content: String, ref: ActorRef) => ref forward s"${content}s"
    }
  }

  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")
//  simpleActor ! "hello actor"

  //1. Messages can be of any type
//  simpleActor ! 42

  case class SpecialMessage(contents: String)
//  simpleActor ! SpecialMessage("Some special content")

  //Rules to sending messages to an actor
  //1. Messages must be IMMUTABLE
  //2. Messages must be SERIALIZABLE
  //In practise to achieve serializable, use case classes or case objects

  //2. Actors have info about their context and themselves
  //This holds info like its path, self or the system it is running on and so on
  //It uses the "context" keyword, which is more like "this" keyword == "context.self" or "self"
  case class SendMessageToYourself(content: String)
//  simpleActor ! SendMessageToYourself("I am a simple actor and I am proud of it")

  //3. Actors reply to messages by using their context.
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val Bob = system.actorOf(Props[SimpleActor], "Bob")

  case class SayHiTo(ref: ActorRef)
//  alice ! SayHiTo(Bob)

  //4. Akka dead letters come in picture when there's no sender
//  alice ! "Hi!"

  //5. Forwarding messages
  //With forwarding, when a message is sent through an actor to another actor, the originator
  //of that message is changed to the actor which forwards the message, we look at an example
  case class WirelessPhoneMessage(content: String, ref: ActorRef)
//  alice ! WirelessPhoneMessage("Hi", Bob)

  //When we use the tell command, the chain is as follows, hence a child has its parent as sender:
  //noSender --> alice --> Bob, sender to alice is noSender, sender to Bob is Alice
  //When we use forward, the root sender is the overall sender of the message
  //noSender --> alice --> Bob, sender to alice and Bob is noSender.


//  EXERCISES.

  //1.Create a counter actor that responds to increment decrement and print messages

  //Domain of the ActorCounter
  object ActorCounter {
    case class Increment(by: Int = 1)
    case class Decrement(by: Int = 1)
    case object PrintValue
  }

  class ActorCounter extends Actor {
    import ActorCounter._

    var value: Int = 0
    override def receive: Receive = {
      case Increment(by) =>
        println(s"Incrementing value by $by")
        value += by
      case Decrement(by) =>
        println(s"Decrementing value by $by")
        value -= by
      case PrintValue => println(s"[Actor Counter]: Current value = $value")
    }
  }

  //1. Output
  import ActorCounter._
  val counter = system.actorOf(Props[ActorCounter], "counter")
//  counter ! Increment()
//  counter ! PrintValue
//
//  counter ! Decrement()
//  counter ! PrintValue
//
//  counter ! Increment(4)
//  counter ! PrintValue
//
//  counter ! Decrement(4)
//  counter ! PrintValue

  (1 to 5).foreach(_ => counter ! Increment())
  counter ! PrintValue
  (1 to 3).foreach(_ => counter ! Decrement())
  counter ! PrintValue

  //2. Create a Bank Account actor that Deposit an amount, withdraw an amount, print stmt
  //In each deposit or withdrawal, actor should reply back a success or failure message

  case class Deposit(amount: Int)
  case class Withdraw(amount: Int)
  case class OperationStatus(status: String)
  case object StatementOfAccount
  object BankAccount{
    def props(acctName: String, balance: Int = 0): Props = Props(new BankAccount(acctName, balance))
  }

  case class BankAccount(acctName: String, var balance: Int = 0) extends Actor {

    override def receive: Receive = {
      case OperationStatus(status) => println(s"[Bank Account] Operation Status: $status")
      case StatementOfAccount =>
        println(s"[Bank Account Statement] Account Name: $acctName, Balance: $balance")
      case Deposit(amount) =>
        balance += amount
        if (balance >= amount) self forward OperationStatus(s"Deposit Success $amount")
        else self forward OperationStatus(s"Deposit Failure $amount")
      case Withdraw(amount) =>
        if (balance < amount) self  forward OperationStatus(s"Withdraw Failure $amount")
        else {
          balance -= amount
          self forward OperationStatus(s"Withdraw Success $amount")
        }
    }
  }

  //2. Output
  val chrisAccount = system.actorOf(BankAccount.props("Chris Eteka"), "chrisAccount")
  val arinzeAccount = system.actorOf(BankAccount.props("Arinze Ogbonna"), "arinzeAccount")

  chrisAccount ! StatementOfAccount
  arinzeAccount ! StatementOfAccount

  chrisAccount ! Deposit(10)
  arinzeAccount ! Deposit(200)

  chrisAccount ! Withdraw(5)
  arinzeAccount ! Withdraw(250)

  chrisAccount ! StatementOfAccount
  arinzeAccount ! StatementOfAccount

  //2. Correction, to be continued
  class BAccount extends Actor {
    import BAccount._

    var funds = 0

    override def receive: Receive = {
      case BDeposit(amount: Int) =>
        if (amount < 0) sender() ! TransactionFailure("Invalid Deposit Amount")
        else {
          funds += amount
        }
    }
  }

  //Domain of the bank account
  object BAccount {
    case class BDeposit(amount: Int)
    case class BWithdraw(amount: Int)
    case object BStatement

    case class TransactionSuccess(message: String)
    case class TransactionFailure(reason: String)
  }
}
