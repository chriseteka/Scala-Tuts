package AkkaInActionSnippets.Tests

import Greeter01Test.testSystem
import akka.actor.{ActorRef, ActorSystem, Props, UnhandledMessage}
import akka.testkit.{CallingThreadDispatcher, EventFilter, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.WordSpecLike

class Greeter01Test extends TestKit(testSystem)
  with WordSpecLike
  with StopSystemAfterAll {

  "Trial test" should {
    "simply succeed, since there is nothing to do" in {
      succeed
    }
  }

  "The Greeter" must {
    "say Hello World! when a Greeting(\"World\") is sent to it" in {
      val dispatcherId = CallingThreadDispatcher.Id
      val props = Props[Greeter].withDispatcher(dispatcherId)
      val greeter = system.actorOf(props)
      EventFilter.info(message = "Hello World!", occurrences = 1).intercept {
        greeter ! Greeting("World")
      }
    }
  }
}

object Greeter01Test {
  val testSystem: ActorSystem = {
    val config = ConfigFactory.parseString(
      """
        akka.loggers = [akka.testkit.TestEventListener]
      """)
    ActorSystem("testSystem", config)
  }
}

import akka.actor.{ActorLogging, Actor}
case class Greeting(message: String)
class Greeter extends Actor with ActorLogging {
  def receive: PartialFunction[Any, Unit] = {
    case Greeting(message) => log.info("Hello {}!", message)
  }
}

object Greeter02 {
  def props(listener: Option[ActorRef] = None) =
    Props(new Greeter02(listener))
}
class Greeter02(listener: Option[ActorRef])
  extends Actor with ActorLogging {
  def receive: PartialFunction[Any, Unit] = {
    case Greeting(who) =>
      val message = "Hello " + who + "!"
      log.info(message)
      listener.foreach(_ ! message)
  }
}

class Greeter02Test extends TestKit(ActorSystem("testsystem"))
  with WordSpecLike
  with StopSystemAfterAll {

  "The Greeter" must {
    "say Hello World! when a Greeting(\"World\") is sent to it" in {
      val props = Greeter02.props(Some(testActor))
      val greeter = system.actorOf(props, "greeter02-1")
      greeter ! Greeting("World")
      expectMsg("Hello World!")
    }
    "say something else and see what happens" in {
      val props = Greeter02.props(Some(testActor))
      val greeter = system.actorOf(props, "greeter02-2")
      system.eventStream.subscribe(testActor, classOf[UnhandledMessage])
      greeter ! "World"
      expectMsg(UnhandledMessage("World", system.deadLetters, greeter))
    }
  }
}

class EchoActorTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with ImplicitSender
  with StopSystemAfterAll {

  "Reply with the same message it receives without ask" in {
    val echo = system.actorOf(Props[EchoActor], "echo2")
    echo ! "some message"
    echo.tell("Another message", testActor)
    expectMsg("some message")
    expectMsg("Another message")
  }
}

class EchoActor extends Actor {
  def receive: PartialFunction[Any, Unit] = {
    case msg =>
      sender() ! msg
  }
}
