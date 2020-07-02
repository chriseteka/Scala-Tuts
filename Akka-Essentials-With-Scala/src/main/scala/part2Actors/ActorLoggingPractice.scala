package part2Actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

//This is useful when writing distributed system for debugging purpose
object ActorLoggingPractice extends App {

  /*
    LOGGING LEVEL:
    1. DEBUG
    2. INFO
    3. WARNING
    4. ERROR
   */

  //1. Explicit method of logging
  class SimpleActorWithExplicitLogger extends Actor {
    val logger = Logging(context.system, this)

    override def receive: Receive = {
      case message => //LOG IT
        logger.info(message.toString)
    }
  }

  //2. Actor Logging
  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a, b) => //LOG IT
        log.info("Two things: {} and {}", a, b) // The brackets will be replaced with the actual val of a and b
      case message => //LOG IT
        log.info(message.toString)
    }
  }

  val system = ActorSystem("LoggingDemo")

  val actor1 = system.actorOf(Props[SimpleActorWithExplicitLogger])
  actor1 ! "A simple message"

  val actor2 = system.actorOf(Props[ActorWithLogging])
  actor2 ! "Logging a simple message by extending a trait"
  actor2 ! ("man", "woman")

}
