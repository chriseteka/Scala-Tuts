package part4faulttolerance

import akka.actor.{Actor, ActorLogging, ActorSystem, PoisonPill, Props}

object ActorLifecycle extends App {

  case object StartChild

  class LifecycleActor extends Actor with ActorLogging {

    /**
      * Observe here that we are overriding some default methods in actor's lifecycle.
      * Observe that the preStart() is called just before an actor starts and the postStop is called after an actor dies
      * This also helps to prove that a child actor dies just before its parent in a case where the parent actor is killed
      */
    override def preStart(): Unit = {
      log.info("I am starting")
    }

    override def postStop(): Unit = {
      log.info("I have stopped")
    }

    override def receive: Receive = {
      case StartChild =>
        context.actorOf(Props[LifecycleActor], "Child")
    }
  }

  val system = ActorSystem("LifecycleDemo")
//  val parent = system.actorOf(Props[LifecycleActor], "Parent")
//  parent ! StartChild
//  parent ! PoisonPill

  /**
    * WE now try to understand the restart lifecycle, in case where an actor fails or throws an exception
    * NB: The actor immediately restarts itself when it encounters an error, this is evident when we try to log
    * messages to the console to verify in each step what happens to the actor, observe the preRestart() and postRestart()
    * So even if an exception is thrown, actors are always restarted (new actors are created that replaces the old actor)
    *
    * This is part of what we call the `Default Supervision strategy`: which states that if an actor throws an excp while
    * processing a message, the message that led to that exception will be removed from the queue and not put back in the
    * mailbox again, and the actor is restarted, with its mailbox untouched and open to receive more messages
    */
  case object Fail
  case object FailChild
  case object Check
  case object CheckChild
  class Child extends Actor with ActorLogging {

    //Called by an actor before it is created for the first time and also called by an old instance actor just before
    //it is swapped with a new one
    override def preStart(): Unit = log.info("Supervised Child started")

    //Called by an actor after it is dead
    override def postStop(): Unit = log.info("Supervised child stopped")

    //Called by the new actor about to replace an old one during swap
    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.info(s"Supervised actor restarting because of ${reason.getMessage}")

    //Called by the new actor that has successfully swapped in, this call is made immediately after the swap
    override def postRestart(reason: Throwable): Unit =
      log.info("Supervised actor restarted")

    override def receive: Receive = {
      case Fail =>
        log.warning("Child will fail now")
        throw new RuntimeException("I failed")
      case Check =>
        log.info("Alive and kicking")
    }
  }

  class Parent extends Actor {

    val child = context.actorOf(Props[Child], "supervisedChild")
    override def receive: Receive = {
      case FailChild =>
        child ! Fail
      case CheckChild =>
        child ! Check
    }
  }

  val supervisor = system.actorOf(Props[Parent], "Supervisor")
  supervisor ! FailChild
  supervisor ! CheckChild
}
