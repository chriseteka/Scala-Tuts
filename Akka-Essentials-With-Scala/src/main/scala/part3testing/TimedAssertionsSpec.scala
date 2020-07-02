package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration._
import scala.util.Random

class TimedAssertionsSpec extends
  TestKit(ActorSystem("TimedAssertionsSpec", ConfigFactory.load().getConfig("specialTimedAssertionConfig")))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {
  import TimedAssertionsSpec._

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  "A worker actor" should {
    val workerActor = system.actorOf(Props[WorkerActor])
    "reply with the meaning of life in a timely manner" in {
      //This reads within at least 500 millis and at most 1 second, this is referred to as a time boxed test.
      //This implies the actor must not reply sooner than 500 millis and not later than 1 second (500 >= n <= 1000)
      within(500 millis, 1 second){
      //This reads within at most 300 millis
//      within(300 millis){
        workerActor ! "work"
        expectMsg(WorkResult(42))
      }
    }

    "reply with a valid work at a reasonable cadence" in {
      //This reads, within a second, all the following in its block should happen
      within(1 second){
        workerActor ! "workSequence"
        //The below reads, within 2secs send 10messages, giving a max interval between these messages as 500millis
        //PS: Test will fail if max time is not big enough to send the amount of specified messages
        //Test will fail if idle time is Zero or too small, as it assumes the message will never be sent
        //Test will fail if number of messages passed is not big enough to satisfy the later assertions
        //returns a seq[result]
        val results: Seq[Int] = receiveWhile[Int](max = 2 second, idle = 500 millis, messages = 10){
          //For each of these messages retrieve a result, which is later collected as a Sequence
          case WorkResult(result) => result
        }

        assert(results.sum > 5)
      }
    }

    //We now try to show that within blocks have no effect on test probes
    "reply to a test probe in a timely manner" in {
      within(1 second) {
        val probe = TestProbe()
        probe.send(workerActor, "work")
        //We try to prove here that the probe doesn't follow the time specified by the within but its own config
        //hence it fails when its timeout from conf is lower than that in the within
        probe.expectMsg(WorkResult(42))
      }
    }
  }
}

object TimedAssertionsSpec {

  case class WorkResult(result: Int)

  //Testing scenario
  class WorkerActor extends Actor {
    override def receive: Receive = {
      case "work" =>
        //lets simulate a long and time intensive computation
        Thread.sleep(500)
        sender() ! WorkResult(42)
      case "workSequence" =>
        //Lets simulate a quick response
        val random = new Random()
        for (i <- 1 to 10) {
          Thread.sleep(random.nextInt(50))
          sender() ! WorkResult(1)
        }
    }
  }
}
