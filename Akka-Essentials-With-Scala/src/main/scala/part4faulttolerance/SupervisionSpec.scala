package part4faulttolerance

import akka.actor.SupervisorStrategy.{Escalate, Restart, Resume, Stop}
import akka.actor.{Actor, ActorRef, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props, StopChild, SupervisorStrategy, Terminated}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

class SupervisionSpec extends TestKit(ActorSystem("SupervisionSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll{
  import SupervisionSpec._

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  //Now we write some tests
  "A supervisor" should {
    "Resume its child in case of a minor fault" in {
      val supervisor = system.actorOf(Props[Supervisor], "supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I love Akka"
      child ! Report
      expectMsg(3)

      child ! "Akka is awesome because i am learning to think in a whole new way"
      child ! Report
      expectMsg(3)
    }

    "Restart its child in case of an empty sentence" in {
      val supervisor = system.actorOf(Props[Supervisor], "supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "I love Akka"
      child ! Report
      expectMsg(3)

      child ! ""
      child ! Report
      expectMsg(0)
    }

    "Terminate the child in case of a major error" in {
      val supervisor = system.actorOf(Props[Supervisor], "supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      //Notice that the test actor has registered for a death watch over the child,
      // hence will be signaled when the child dies
      watch(child)
      child ! "akka is nice" //This messaged leads to the death of the child

      //The Terminated message is now sent to the test actor, and by using the expectMessageType[],
      //we have extracted the contents in it and assigned it to a variable, the asserted afterwards.
      val terminatedMessage = expectMsgType[Terminated]
      assert(terminatedMessage.actor == child)
    }

    "Escalate an error when it doesn't know what to do" in {
      val supervisor = system.actorOf(Props[Supervisor], "supervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      watch(child)
      child ! 43
      val terminatedMessage = expectMsgType[Terminated]
      assert(terminatedMessage.actor == child)

    }
  }

  "A kinder supervisor" should {
    "Not kill children in case it's restarted or escalates failures" in {
      val supervisor = system.actorOf(Props[NoDeathOnRestartSupervisor], "noDeathSupervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      child ! "Akka is cool"
      child ! Report
      expectMsg(3)

      child ! 45
      child ! Report
      expectMsg(0)
    }
  }

  "An all for one supervisor" should {
    "apply the all-for-one-strategy" in {
      val supervisor = system.actorOf(Props[AllForOneSupervisor], "AllForOneSupervisor")
      supervisor ! Props[FussyWordCounter]
      val child = expectMsgType[ActorRef]

      supervisor ! Props[FussyWordCounter]
      val secondChild = expectMsgType[ActorRef]

      secondChild ! "Testing supervision"
      secondChild ! Report
      expectMsg(2)

      //The event filter asserts that the child throws the expected (nullPointer) exception
      EventFilter[NullPointerException]() intercept {
        //Doing this leads to a null pointer exception, which should cause a restart,
        // only this time, both child are restarted
        child ! ""
      }

      //This proves that the second child restarts and loss its internal state which was 2 earlier.
      //We had to sleep a little to allow the supervision strategy restart all the children, before proceeding
      Thread.sleep(500)
      secondChild ! Report
      expectMsg(0)
    }
  }

}

object SupervisionSpec {

  case object Report

  /**
    * Recall that the way a parent acts by default to an exception thrown by its child is to restart it,
    * We however will be looking at a different strategy whenever an exception happens in a child by overriding the
    * supervisors strategy to handle all exceptions that it child might throw and specifying what becomes of the child
    */
  class Supervisor extends Actor {

    override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy(){
      //This is where the decisions are made when any kind of exceptions are encountered
      //An object from Directive, the trait Directives are possible thing that an actor could be told to do when it fails
      case _: NullPointerException => Restart //Clears all internal state of the actor that fails because the instance
        //of that actor is been replaced with another one when the restart directive is called upon.
      case _: IllegalArgumentException => Stop //Kills the failed child and its children
      case _: RuntimeException => Resume //Keeps internal state of the failed actor that is been resumed
      case _: Exception => Escalate //Kills the failed child and all it children then escalates
      // to the user guardian of the failed child with a message the child spits just before dying,
      //hence leading to the restart of the whole system, because by default when a user guardian get such exception,
      //its default strategy is to restart the entire system, it achieves this by making the failed child's supervisor
      //to restart all its children just before restarting the supervisor itself.
    }

    override def receive: Receive = {
      case props: Props =>
        val childRef = context.actorOf(props)
        sender() ! childRef
    }
  }

  //The below implementation makes it such that when the user guardian try to restart this supervisor, its children are
  //not restarted, remember that restart implies replacing old instance of an actor with a new one within an actorRef
  //This is because the preRestart() method has been override to do nothing, this however is not its original impl
  //This however clears the state of all the children under this supervisor, but does not kill, remove and replace them
  class NoDeathOnRestartSupervisor extends Supervisor {
    override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
      //Do nothing
    }
  }

  class AllForOneSupervisor extends Supervisor {

    /**
      * This is different from the OneForOne in the sense that the one for one only pays attention,
      *  by applying the supervision strategy on the particular actor that cause the failure (more like locally scoped).
      *  Meaning that only the failed actor is been worked on based on the directive specified in the strategy
      *  While the AllForOne applies same strategy for all actor, regardless of the actor that caused the failure
      *  (more like global scope). Meaning every actor will suffer same fate as the failed actor,
      *  not minding whether they caused the problem or not.
      */
    override val supervisorStrategy: AllForOneStrategy = AllForOneStrategy() {
      case _: NullPointerException => Restart
      case _: IllegalArgumentException => Stop
      case _: RuntimeException => Resume
      case _: Exception => Escalate
    }
  }

  class FussyWordCounter extends Actor {

    var words = 0

    override def receive: Receive = {
      case Report => sender() ! words
      case "" => throw new NullPointerException("sentence is empty")
      case sentence: String =>
        if (sentence.length > 20) throw new RuntimeException("Sentence is too big")
        else if (!Character.isUpperCase(sentence(0)))
          throw new IllegalArgumentException("Sentence must start with upper case")
        else words += sentence.split(" ").length
      case _ => throw new Exception("can only receive string")
    }
  }
}
