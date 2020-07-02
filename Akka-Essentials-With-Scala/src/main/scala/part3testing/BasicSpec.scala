package part3testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration._
import scala.util.Random

//Recommended to name your test classes with the post-fix "Spec"
//Extending TestKit and passing param ActorSystem, makes the test run by first instantiating an actor system
class BasicSpec extends TestKit(ActorSystem("BasicSpec"))
  with ImplicitSender //Helps in send reply scenarios in actors
  with WordSpecLike //Helps write our test like english (BDD)
  with BeforeAndAfterAll //Provides us with hooks that we can run before and/or after
{
  import BasicSpec._

  //Below is a sample hook that runs after all tests, the method below is the tear down method
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  //Below is the general structure of a test

  //Test Suite
  "The thing being tested" should{

    //Tests
    "do this" in {
      //testing scenario
    }

    //Tests
    "do another thing" in {
      //testing scenario

    }
  }

  //Now for our real tests
  "A Simple Actor" should {
    "send back the same message" in {
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "Hello Test"
      echoActor ! message

      //We no assert using expectMsg() method
      //This method fails if it waits for a message for 3sec and does not receive it, it times out and fails
      //This is configurable though using: "akka.test.single-expect-defalt"
      expectMsg(message)
    }
  }

  "A Black Hole Actor" should {
    "send back some message" in {
      val blackHoleActor = system.actorOf(Props[BlackHole])
      val message = "Hello Test"
      blackHoleActor ! message

      //We no assert using expectMsg() method
      //This method fails if it waits for a message for 3sec and does not receive it, it times out and fails
      //This is configurable though using: "akka.test.single-expect-defalt"
//      expectMsg(message)

      //Opposite of expectMsg(), we specify wait time here and import scala.concurrent.duration
      expectNoMessage(1 second)
    }
  }

  /**
    * LETS NOW TALK ABOUT MESSAGE ASSERTIONS
    */
  "A Lab Test Actor" should {
    //This makes this actor global across all the tests in this suite
    //However if the Actors are stateful, then they can be created in each test and not global,
    // so as not to have unexpected values
    val labTestActor = system.actorOf(Props[LabTestActor])

    "Turn a string into upper case" in {
      labTestActor ! "I love akka"
      // Helps us get hold of the reply, and then do assertions after that
      //This is unlike the expectMsg() method that simply checks for equality
      val reply = expectMsgType[String]
      assert(reply == "I LOVE AKKA")
    }

    "Reply to a greeting" in {
      labTestActor ! "greeting"
      expectMsgAnyOf("Hi", "Hello")
    }

    "Reply with favorite tech" in {
      labTestActor ! "favoriteTech"
      expectMsgAllOf("Scala", "Akka")
    }

    "Reply with cool tech in a different way" in {
      labTestActor ! "favoriteTech"
      //This returns a seq[Any], with the messages received,
      // This helps us get hold of the returns and then continue our assertions
      val messages = receiveN(2)
    }

    "Reply with cool tech in a fancy way" in {
      labTestActor ! "favoriteTech"

      //The Partial Function gives us the room to do more with the values it has in it.
      //It may house many value, but we only care about the cases which we define within the PF
      //It is the most powerful
      expectMsgPF() {
        case "Scala" =>
        case "Akka" =>
      }
    }
  }

}

//As a habit, always create companion objects for your tests, just like you do in actors domain
//We can store all val, methods, or info that will be used in the test
//The structure of testing written here will be our pattern for writing tests
object BasicSpec {

  //We will use this simple actor to do asynchronous assertions
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender() ! message
    }
  }

  class BlackHole extends Actor {
    override def receive: Receive = Actor.emptyBehavior
  }

  class LabTestActor extends Actor {
      val random = new Random()

    override def receive: Receive = {
      case "greeting" => if (random.nextBoolean()) sender() ! "Hi" else sender() ! "Hello"
      case "favoriteTech" =>
        sender() ! "Scala"
        sender() ! "Akka"
      case message: String => sender() ! message.toUpperCase()
    }
  }
}

/**
  * A simple question can be who receives the messages sent or who is the sender
  * `testActor`: is a member of testKit and is the actor that is responsible for sending and receiving messages from
  * our test actors, this comes implicitly because we mixed in the `implicitSender` trait
  */
