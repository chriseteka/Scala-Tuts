package AkkaInActionSnippets.Tests

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, EventFilter, ImplicitSender, TestActorRef, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.util.Random

class AkkaInActionTest extends TestKit(ActorSystem("AkkaInActionTest"))
  with ImplicitSender
  with WordSpecLike
  with MustMatchers
  with StopSystemAfterAll {
  import AkkaInActionTest._

  //STRUCTURE, SIMILAR TO THAT OF DANIEL'S VIDEO, EXCEPT HERE, WE USE `MUST` IN THE TEST SUITES, AS AGAINST `SHOULD`
  "A Silent Actor" must {

    "change state when it receives a message, single threaded" in {
      //Write the test, succeed, or fail
//      fail("not implemented yet")
//      succeed
      val silentActor = TestActorRef[SilentActor]
      silentActor ! SilentMessage("whisper")
      silentActor.underlyingActor.state must contain("whisper")
    }

    "change internal state when it receives a message, multi" in {

      val silentActor = system.actorOf(Props[SilentActor], "s3")
      silentActor ! SilentMessage("whisper1")
      silentActor ! SilentMessage("whisper2")
      silentActor ! GetState(testActor)
      expectMsg(Vector("whisper1", "whisper2"))
    }

    "change state when it receives a message, multi-threaded" in {
      //Write the test, succeed, or fail
//      fail("not implemented yet")
      succeed
    }
  }

  "A Sending Actor" must {
    "send a message to another actor when it has finished processing" in {
      import SendingActor._
      val props = SendingActor.props(testActor)
      val sendingActor = system.actorOf(props, "sendingActor")
      val size = 1000
      val maxInclusive = 100000
      def randomEvents() = (0 until size).map{ _ =>
        Event(Random.nextInt(maxInclusive))
      }.toVector
      val unsorted = randomEvents()
      val sortEvents = SortEvents(unsorted)
      sendingActor ! sortEvents
      expectMsgPF() {
        case SortedEvents(events) =>
          events.size must be(size)
          unsorted.sortBy(_.id) must be(events)
      }
    }

    "filter out particular messages" in {
      import FilteringActor._
      import scala.concurrent.duration._

      val props = FilteringActor.props(testActor, 5)
      val filter = system.actorOf(props, "filter-1")
      filter ! Event(1)
      filter ! Event(2)
      filter ! Event(1)
      filter ! Event(3)
      filter ! Event(1)
      filter ! Event(4)
      filter ! Event(5)
      filter ! Event(5)
      filter ! Event(6)
      filter ! Event(7)
      val eventIds = receiveWhile() {
        case Event(id) if id <= 5 => id
      }
      eventIds must be(List(1, 2, 3, 4, 5))
      expectMsg(Event(6))
    }

    "filter out particular messages using expectNoMsg" in {
      import FilteringActor._
      val props = FilteringActor.props(testActor, 5)
      val filter = system.actorOf(props, "filter-2")
      filter ! Event(1)
      filter ! Event(2)
      expectMsg(Event(1))
      expectMsg(Event(2))
      filter ! Event(1)
      expectNoMessage
      filter ! Event(3)
      expectMsg(Event(3))
      filter ! Event(1)
      expectNoMessage
      filter ! Event(4)
      filter ! Event(5)
      filter ! Event(5)
      expectMsg(Event(4))
      expectMsg(Event(5))
      expectNoMessage
    }
  }
}

//A style I have learnt from daniel, this may not be needed, as you can always import objects and/or classes needed for
//your test without having to define them as daniel always said, for now we continue with this
object AkkaInActionTest {

  case class SilentMessage(message: String)
  case class GetState(receiver: ActorRef)

  class SilentActor extends Actor {

    var internalState: Vector[String] = Vector[String]()

    def receive: PartialFunction[Any, Unit] = {
      case SilentMessage(data) =>
        internalState = internalState :+ data
      case GetState(receiver) => receiver ! internalState
    }
    def state: Vector[String] = internalState
  }

}

object SendingActor {
  def props(receiver: ActorRef) = Props(new SendingActor(receiver))
  case class Event(id: Long)
  case class SortEvents(unsorted: Vector[Event])
  case class SortedEvents(sorted: Vector[Event])
}

class SendingActor(receiver: ActorRef) extends Actor {

  import SendingActor._

  def receive: Receive = {
    case SortEvents(unsorted) =>
      receiver ! SortedEvents(unsorted.sortBy(_.id))
  }
}

object FilteringActor {
  def props(nextActor: ActorRef, bufferSize: Int) = Props(new FilteringActor(nextActor, bufferSize))
  case class Event(id: Long)
}
class FilteringActor(nextActor: ActorRef, bufferSize: Int) extends Actor {
  import FilteringActor._
  var lastMessages: Vector[Event] = Vector[Event]()
  def receive: PartialFunction[Any, Unit] = {
    case msg: Event =>
      if (!lastMessages.contains(msg)) {
        lastMessages = lastMessages :+ msg
        nextActor ! msg
        if (lastMessages.size > bufferSize) {
          // discard the oldest
          lastMessages = lastMessages.tail
        }
      }
  }
}
