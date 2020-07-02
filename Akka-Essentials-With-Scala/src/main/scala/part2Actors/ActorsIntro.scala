package part2Actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {

  //All apps should have only one global actor system, which can hold many other actors
  //Actors are uniquely IDed with a unique name
  val actorSystem = ActorSystem("FirstActorSystem")

  //Now we create other actors
  //Word count actor
  class WordCounterActor extends Actor {

    //Internal data
    var totalWords = 0

    //Behaviour/ Receive handler
    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[Word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[Word counter]: I cannot understand ${msg.toString}")
    }
  }

  //We now instantiate our actor, we do this by invoking the actor system, as follows:
  //This returns an actorRef which is what you can only use to communicate with the actor
  val wordCounter = actorSystem.actorOf(Props[WordCounterActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCounterActor], "anotherWordCounter")

  //Now we communicate with the actor we just created as follows
//  wordCounter.!("I am learning akka and its pretty damn cool")
  //Sending this message is completely asynchronous
  wordCounter ! "I am learning akka and its pretty damn cool"
  anotherWordCounter ! "A different message"

  //The exclamation method ! know as tell is the only way to communicate with an actor
  //Now lets look at how to instantiate an actor which has constructor arguments
  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
      case _ => println("Am confused")
    }
  }

  object Person{
//    def props(name: String): Props = Props(new Person(name))
    def props(name: String): Props = Props(new Person(name))
  }

  //Although this is legal, but it is not encouraged. A better way will be to define a
  //companion object of class Person, with a method which returns a props of Person instantiated
  val person = actorSystem.actorOf(Props(new Person("Chris")), "personActor")
  person ! "hello"

  //This is a more encouraged pattern to create a prop which instantiates an object with
  //constructor arguments.
  val person2 = actorSystem.actorOf(Person.props("Eteka"), "person2Actor")
  person2 ! "hi"
}
