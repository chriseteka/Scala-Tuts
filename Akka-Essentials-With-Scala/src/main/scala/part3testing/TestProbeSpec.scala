package part3testing

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

class TestProbeSpec extends TestKit(ActorSystem("TestProbeSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll
{
  import TestProbeSpec._

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  "A master actor" should {
    "Register a slave" in {
      val master = system.actorOf(Props[Master])
      //TestProbe: is a special actor with some assertion capabilities, its like the `testActor`, that comes by default,
      //having so many assert capability like the once we have seen before
      val slave = TestProbe("Slave")

      master ! Register(slave.ref)
      expectMsg(RegistrationAck)
    }

    "send the work to the slave actor" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("Slave")
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)

      val workLoadString = "I love Akka"
      master ! Work(workLoadString)

      //Test of the interaction between the master and an imaginary slave (TestProbe)
      slave.expectMsg(SlaveWork(workLoadString, testActor))
      //The nicest thing about it, is that they can be instructed to receive and reply to a message even though they are
      //imaginary and do not really exist, look at this below
      slave.reply(WorkCompleted(3, testActor))

      //This is similar to saying testActor.expectMsg(), since it was the one that first started the interaction,
      //and is the one that was referred to as original requester from our master actor that was defined below
      expectMsg(WorkReport(3))
    }

    "aggregate data correctly" in {
      val master = system.actorOf(Props[Master])
      val slave = TestProbe("Slave")
      master ! Register(slave.ref)
      expectMsg(RegistrationAck)

      val workLoadString = "I love Akka"
      val sham = "I love Akka"
      master ! Work(workLoadString) // This cycle should return count as 3, after counting the string once
      master ! Work(workLoadString) // This cycle should return count as 6, after counting the string the second time
      master ! Work(sham)

      //lets assume that we do not have a slave actor, although having created an imaginary one (TestProbe),
      //we have not sent it any message or make it reply as we did from the test above this.
      //This states that, as long as slave receives SlaveWork(workloadString, testActor), always reply back with
      //WorkCompleted(3, testActor). Observe how we have use backticks in the case stmt.
      //Hence if the slave fails the above condition during the course of running the test, it will fail
      slave.receiveWhile() {
        case SlaveWork(`workLoadString`, `testActor`) => slave.reply(WorkCompleted(3, testActor))
      }
      expectMsg(WorkReport(3))
      expectMsg(WorkReport(6))
      expectMsg(WorkReport(9))
    }
  }

}

object TestProbeSpec {
  //Scenario
  /*
    word counting actor hierarchy master-slave
    send some work to the master, the master sends the slave the piece of work,
    the slave processes the work and replies to the master
    the master aggregates the results and reply to the outside world (original requester)
   */

  case class Register(slaveRef: ActorRef)
  case object RegistrationAck
  case class Work(text: String)
  case class SlaveWork(text: String, originalRequester: ActorRef)
  case class WorkCompleted(count: Int, originalRequester: ActorRef)
  case class WorkReport(result: Int)

  class Master extends Actor {
    override def receive: Receive = {
      case Register(slaveRef) =>
        sender() ! RegistrationAck
        context.become(online(slaveRef, 0))
      case _ => //Do nothing
    }

    def online(slaveRef: ActorRef, totalWordCount: Int): Receive = {
      case Work(text) => slaveRef ! SlaveWork(text, sender())
      case WorkCompleted(count, originalRequester) =>
        val newTotalWordCount = totalWordCount + count
        originalRequester ! WorkReport(newTotalWordCount)
        context.become(online(slaveRef, newTotalWordCount))
    }
  }

  //Assuming tha yes, a slave actor was created, lets say we dont care about it, since all our concern is to test
  //just the master actor at the moment, so lets leave this not defined for now.
  //Since not all that is required by the master is provided to it, we need a test probe which holds the place for
  // a potential, non existing entities that will be used for testing.

}
