package part4faulttolerance

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Kill, PoisonPill, Props, Terminated}

/**
  * WE ARE CONSIDERING THE CONCEPT `SUPERVISION AND MONITORING`
  */
object StartingStoppingActors extends App {

  val system = ActorSystem("StoppingActorsDemo")

  object Parent {
    case class StartChild(name: String)
    case class StopChild(name: String)
    case object Stop
  }

  class Parent extends Actor with ActorLogging {
    import Parent._

    override def receive: Receive = withChildren(Map())

    def withChildren(children: Map[String, ActorRef]): Receive = {
      case StartChild(name) =>
        log.info(s"Starting child with name: $name")
        context.become(withChildren(children + (name -> context.actorOf(Props[Child], name))))
      case StopChild(name) =>
        log.info(s"Stopping Child with the name: $name")
        val childOption = children.get(name)

        /**
          * NB: context.stop(child) is a non blocking method, it happens asynchronously, what you just did is,
          * you sent a signal to the child actor, asking it to stop,
          * that however does not mean that it stops it immediately.
          * This implies that, after calling the stop method, the actor doesn't stop at once, it can even execute some
          * requests before stopping, observe the sample we have given below, after initiating a stop
          */
        childOption.foreach(childRef => context.stop(childRef))
      case Stop =>
        log.info("Stopping myself")

        /**
          * This is asynchronous as explained above, however, stopping a parent actor stops all its children as well.
          * It is important to note however that all children will be stopped first before its parent
          */
        context.stop(self)
      case raw =>
        log.info(raw.toString)
    }
  }

  class Child extends Actor with ActorLogging{
    override def receive: Receive = {
      case message =>
        log.info(message.toString)
    }
  }

  import Parent._

  /**
    * WE HAVE ESTABLISHED THEREFORE THAT THE FIRST WAY OF STOPPING ACTOR IS BY USING: context.stop(actorRef)
    */
//  val parent = system.actorOf(Props[Parent], "parent")
//  parent ! StartChild("Child1")
//  val child = system.actorSelection("/user/parent/Child1")
//  child ! "Hi Kid"
//  parent ! StopChild("Child1")
////  for (_ <- 1 to 50) child ! "Are you still there??"
//
//  parent ! StartChild("Child2")
//  val child2 = system.actorSelection("/user/parent/Child2")
//  child2 ! "Hi second child"
//
//  parent ! Stop
//  //This will not be received, because this message comes in after the stopping signal
//  for (_ <- 1 to 10) parent ! "Parent are you still there??"
//  for (i <- 1 to 100) child2 ! s"[$i] Second Child, are you still Alive"

  /**
    * METHOD 2: USING SPECIAL MESSAGES (messages that all actors understands that stops them as a result) ->
    * 1. PoisonPill (Actor will terminate without any exception)
    * 2. Kill (Actor terminates with an exception: `akka.actor.ActorKilledException`)
    * None of the above messages can be handled in you message receive handler, because they are special
    */
//  val looseActor = system.actorOf(Props[Child])
//  looseActor ! "hello, loose actor"
//  looseActor ! PoisonPill
//  looseActor ! "Are you still there."
//
//  val abruptlyTerminatedActor = system.actorOf(Props[Child])
//  abruptlyTerminatedActor ! "You are about to be terminated"
//  abruptlyTerminatedActor ! Kill
//  abruptlyTerminatedActor ! "Are you still there??"

  /**
    * LETS CONSIDER A CONCEPT KNOWN AS `DEATH WATCH` (A mechanism of checking when an actor dies)
    */
  class Watcher extends Actor with ActorLogging {
    import Parent._

    override def receive: Receive = {
      case StartChild(name) =>
        val child = context.actorOf(Props[Child], name)
        log.info(s"Started and watching child with name: $name")

        /**
          * This registers the parent actor for the death of the child, so when the child dies, this actor will receive
          * a special message `Terminated(refToDeadChild)` sent from akka, and this message can be handled in our cases.
          * This is what the context.watch(refToChild) does.
          * The opposite to this is the context.unwatch(refToChild), which de-lists this actor from being watched,
          * hence no monitor is over the child anymore.
          *
          * NB: This can work on any number of actors that may or may not be your children
          * NB: When u register for a death watch on an actor that is already dead,
          *   you will still receive the terminated message.
          */
        context.watch(child)
      case Terminated(ref) =>
        log.info(s"The reference that am watching $ref has been stopped/dead.")
    }
  }

  val watcher = system.actorOf(Props[Watcher], "watcher")
  watcher ! StartChild("watchedChild")
  val child = system.actorSelection("/user/watcher/watchedChild")
  Thread.sleep(500)
  child ! PoisonPill

}
