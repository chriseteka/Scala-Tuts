package part3testing

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

//We intercept log messages in most case when we need to do integration test and it is hard to inject TestProbes into
//the akka architecture. This style of testing is the best when you ever need to do integration test of your dist. sys
class InterceptingLogsSpec
  extends TestKit(ActorSystem("InterceptingLogsSystem", ConfigFactory.load().getConfig("interceptingLogMessages")))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll
{
  import InterceptingLogsSpec._

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  val item: String = "Rock the JVM course"
  val creditCard: String = "1234-1234-1234-1234"
  val invalidCreditCard: String = "0000-0000-0000-0000"

  //Now we define our test suite
  "A checkout flow" must {
    "correctly log the dispatch of an order" in {
      /**
        * The below code now inspects all logs from our checkout actor and looks for a message of that pattern,
        * bearing in mind that this message is logged out just one time. This is the way we test that the actor
        * executes successfully with the required output result after communicating with other child actors
        * By default, the event listener intercept times out in 3sec while waiting for a message on the log,
        * so if the processes that results to this message takes time, then we expect the test to fail.
        * This is however reconfigurable with the key "akka.test.filter-leeway"
        * Note also that these log can be in any kind of level
        *
        * */
       EventFilter.info(pattern = s"Order [0-9]+ for item $item has been dispatched.", occurrences = 1) intercept {
         //Now our test code
         val checkoutRef = system.actorOf(Props[CheckoutActor])
         checkoutRef ! Checkout(item, creditCard)
       }
    }

    "freak out if the payment is denied" in {
      //We use this test to show that the event filter can be parameterized, so it expects a certain kind of value
      EventFilter[RuntimeException](occurrences = 1) intercept {
        //Now our test code
        val checkoutRef = system.actorOf(Props[CheckoutActor])
        checkoutRef ! Checkout(item, invalidCreditCard)
      }
    }
  }

}

object InterceptingLogsSpec {

  case class Checkout(item: String, creditCard: String)
  case class AuthorizeCard(creditCard: String)
  case class DispatchOrder(item: String)
  case object PaymentAccepted
  case object PaymentDenied
  case object OrderConfirmed

  /**
    * THE FLOW OF THIS SAMPLE IS SUCH THAT WHEN YOU WANT TO CHECKOUT STOCK IN YOUR CART AND ABOUT TO MAKE PAYMENT,
    * 1. THE CHECKOUT ACTOR WILL SEND A REQUEST TO THE PAYMENT MANAGER ACTOR TO VALIDATE YOUR PAYMENT DETAILS
    * 2. THIS PAYMENT MANAGER CAN NOW REPLY TO THE CHECKOUT ACTOR WITH A RESPONSE OF ACCEPTED OR DENIED
    * 3. WHEN EVERYTHING IS ACCEPTED, THEN THE CHECKOUT ACTOR TELLS THE FULFILMENT MANAGER ACTOR TO DISPATCH YOUR ORDER
    * 4. AFTER DISPATCHING, THE CHECKOUT ACTOR IS NOTIFIED AGAIN AND IS RETURNED TO ITS NORMAL STATE TO TAKE NEW ORDERS
    *
    * THE BELOW CODE AND ARCHITECTURE IS DIFFICULT TO TEST FOR THE FOLLOWING REASONS:
    *   1. THERE ARE TWO CHILD ACTORS THAT NEEDS TO BE CREATED BY THE CHECKOUT ACTOR,
    *     THIS IS NOT EASY TO INJECT USING OUR TEST PROBES
    *   2. OBSERVE THAT THE CHECKOUT ACTOR DOESN'T REPLY TO THE OUTSIDE WORLD, AND HENCE IT MAY BE DIFFICULT TO TEST
    *     THAT THE THE PROCESS IS COMPLETE AND THE RIGHT OUTPUT HAVE BEEN RECEIVED
    *
    * IT IS FOR CASES LIKE THIS THAT WE CAN USE THE TEST KIT'S `EVENT FILTERS`
    */

  class CheckoutActor extends Actor {

    //We start by creating two child actors:
    private val paymentManager = context.actorOf(Props[PaymentManager])
    private val fulfilmentManager = context.actorOf(Props[FulfilmentManager])

    override def receive: Receive = awaitingCheckout

    //We now create other receive methods as thus:
    def awaitingCheckout: Receive = {
      case Checkout(item, card) =>
        paymentManager ! AuthorizeCard(card)
        context.become(pendingPayment(item))
    }

    def pendingPayment(item: String): Receive = {
      case PaymentAccepted =>
        //We can now proceed to fulfilment part
        fulfilmentManager ! DispatchOrder(item)
        context.become(pendingFulfilment(item))

      case PaymentDenied =>
        throw new RuntimeException("I can't handle this anymore")

    }

    def pendingFulfilment(str: String): Receive = {
      case OrderConfirmed =>
        //Observe how we changed our actor state to become open again to receiving new messages
        context.become(awaitingCheckout)
    }
  }

  class PaymentManager extends Actor {
    
    override def receive: Receive = {
      case AuthorizeCard(card) =>
        if (card.startsWith("0")) sender() ! PaymentDenied
        else{
          Thread.sleep(4000) //We use this to verify that the event listener only waits 3sec for a response
          sender() ! PaymentAccepted
        }
    }
  }

  class FulfilmentManager extends Actor with ActorLogging {
    private val orderId = 43
    override def receive: Receive = onMessage(orderId)

    private def onMessage(orderId: Int): Receive = {
      case DispatchOrder(item) =>
        context.become(onMessage(orderId + 1))
        log.info(s"Order $orderId for item $item has been dispatched.")
        sender() ! OrderConfirmed
    }
  }
}

//IT IS WORTH MENTIONING THAT ALL THE TESTS WE HAVE BEEN WRITING ARE ASYNCHRONOUS.
