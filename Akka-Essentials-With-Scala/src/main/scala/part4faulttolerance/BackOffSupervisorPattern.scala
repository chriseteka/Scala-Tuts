package part4faulttolerance

import java.io.File

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}

import scala.io.Source

object BackOffSupervisorPattern extends App {

  case object ReadFile
  class FileBasedPersistenceActor extends Actor with ActorLogging {

    var datasource: Source = null

    override def preStart(): Unit =
      log.info("Persistence actor starting")

    override def postStop(): Unit =
      log.warning("Persistence actor stopped")

    override def preRestart(reason: Throwable, message: Option[Any]): Unit =
      log.warning("Persistence actor restarting")

    override def receive: Receive = {
      case ReadFile =>
        if (datasource == null)
          datasource = Source.fromFile(new File("src/main/resources/testfiles/important-data.txt"))
        log.info(s"I've just read some important data: ${datasource.getLines().toList}")
    }
  }

  val system = ActorSystem("BackOffSupervisorDemo")
  val simpleActor = system.actorOf(Props[FileBasedPersistenceActor], "SimpleActor")

  simpleActor ! ReadFile
}
