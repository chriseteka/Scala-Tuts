package part5infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, PoisonPill, Props}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

object SchedulersAndTimers extends App {

  class OneSecActor extends Actor with ActorLogging {

    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("SchedulersTimersDemo")
  val oneSecActor = system.actorOf(Props[OneSecActor], "oneSecActor")
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  val killOnIdle: Cancellable = system.scheduler.schedule(1 second, 1 seconds){
    oneSecActor ! PoisonPill
  }
}
