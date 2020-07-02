package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, TestActorRef, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration.Duration

class SynchronousTestingSpec extends WordSpecLike with BeforeAndAfterAll {
  import SynchronousTestingSpec._

  implicit val system = ActorSystem("SynchronousTestingSpec")

  override def afterAll(): Unit = system.terminate()

  //Now our test suite
  "A counter" should {
    "synchronously increase its counter" in {
      val counter = TestActorRef[Counter](Props[Counter])
      //The test actor ref ensures that at any point a message is sent to an actor, it is already received
      //Hence you are sure on the first value in the Counter and certain on its output, which is what test is all about.
      //This is so because, sending a message to test actor ref, happens in that thread that calls it,
      // as against the async form which may (in most cases) will designate another thread to do this,
      // hence jeopardizing the order in which messages are sent into this actor.
      counter ! Inc
      assert(counter.underlyingActor.count == 1)
    }

    //We use this test to show that the test actor ref allows us to poke the actor and call methods on it in a safe way
    "synchronously increase its counter at the call of the receive function" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter.receive(Inc)
      assert(counter.underlyingActor.count == 1)
    }

    //We can also achieve synchronous calls/tests by using the calling thread dispatcher,
    // which ensures that same thread that instantiates and sends a message is used to investigate the actor.
    "work on the calling thread dispatcher" in {
      //This is a way to configure in a dispatcher into a props of an actor, to achieve synchronous actor behaviour
      //without specifying the dispatcher like we did, then running this test will fail because the probe will have
      //to wait a couple second before it receives a message from the Counter
      val counter = system.actorOf(Props[Counter].withDispatcher(CallingThreadDispatcher.Id))
      val probe = TestProbe()
      //This ensures that operations on the counter actor will be carried out by the thread that the probe runs on
      probe.send(counter, Read) //Therefore, the count result is immediately passed onto the probe
      probe.expectMsg(Duration.Zero, 0) //A confirmation that probe has already received the message from counter
    }
  }
}

object SynchronousTestingSpec {

  case object Inc
  case object Read


  class Counter extends Actor {

    var count = 0
    override def receive: Receive = {
      case Inc => count += 1
      case Read => sender() ! count
    }
  }
}

/**
  * THE ABOVE STYLE OF TESTING GIVES ISSUES IF RESULT ARE DEPENDENT ON SOME PERSISTENCE WHICH TAKES TIME TO RETURN, OR
  * ON ACTORS THAT EXHIBITS ASYNCHRONOUS WAY OF REACTING TO MESSAGES.
  * ONE NEEDS TO BE CAREFUL OF MIXING SOME OTHER UTILITIES ON THINGS THAT ACTS ON THE SAME CALLING THREAD:
  *   THIS MEANS THAT FOR SOME ACTORS THAT REPLIES TO THEIR CALLERS, IF WRITTEN IN A SYNCHRONOUS FASHION MAY LEAD
  *   TO A DEADLOCK WHICH WE DO NOT WANT.
  */
