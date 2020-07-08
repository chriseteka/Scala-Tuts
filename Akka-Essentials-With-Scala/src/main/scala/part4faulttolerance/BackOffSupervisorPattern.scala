package part4faulttolerance

import java.io.File

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorLogging, ActorSystem, OneForOneStrategy, Props}
import akka.pattern.{Backoff, BackoffSupervisor}

import scala.io.Source
import scala.concurrent.duration._

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
//  val simpleActor = system.actorOf(Props[FileBasedPersistenceActor], "SimpleActor")
//
//  simpleActor ! ReadFile

  /**
    * THE BACK OFF SUPERVISOR PATTERN HELPS TO ENSURE THAT WHENEVER ANY EXTERNAL RESOURCE WHICH OUR ACTOR DEPENDS ON IS
    * UNAVAILABLE, THE ACTORS ARE NOT RESTARTED ALL AT ONCE OR EVEN OVERLOAD THIS RESOURCE WHEN IT IS BACK ON, E.G OF
    * THIS KIND OF A RESOIRCE IS A DATABASE.
    *
    * HIS PATTERN ALSO HELP TO ENSURE THAT OUR ACTOR DOES NOT KEEP RESTARTING DUE TO FAILURE OF AN EXTERNAL RESOURCE
    * RATHER, IT WOULD EXHIBIT SOME AMOUNT OF RANDOMNESS WHEN IT TRIES TO DO SO AND AT SPECIFIED TIME INTERVALS.
    */

  val simpleSupervisorProps = BackoffSupervisor.props( //-> We define a props as follows
    Backoff.onFailure( //-> This props would react on failure
      Props[FileBasedPersistenceActor], //-> The actor been watched over is this
      "simpleBackoffActor", //-> A name is given to the actor
      3 seconds, //-> minimum wait time before the actor is restarted, on failure again the min time become 2x
      30 seconds, //-> maximum amount of time this actor will keep trying to restart before it stops
      0.2 //-> Randomness to the time, so that all actors does not start at same time
    )
  )

  /*
    Creates an actor from the props above with a name simpleSupervisor.
    The simpleSupervisor creates in itself a child called the simpleBackoffActor which is spawned from the FileBasedPActor
    Hence under the simple supervisor parent is a FileBasedPersistentActor.
    Any message received by the simple supervisor is forwarded to its child.
    NB: For now the supervision strategy is DEFAULT (RESTARTS THE ACTOR ON ANYTHING, ERROR, EXCEPTION, ETC).
      This strategy however follows the rules above, its first attempt to restart must kick in after 3sec
      - next attempt will be in 2x previous attempt, hence in the order of 3, 6, 12, 24 (last attempt)
      - If in total of 30sec the actor fails to restart, it will not try to restart again
   */
//  val simpleBackoffSupervisor = system.actorOf(simpleSupervisorProps, "simpleSupervisor")
//  simpleBackoffSupervisor ! ReadFile

  val stopSupervisorProps = BackoffSupervisor.props( //-> We define a props as follows
    Backoff.onStop( //-> This props would react on stop
      Props[FileBasedPersistenceActor], //-> The actor been watched over is this
      "StopBackoffActor", //-> A name is given to the actor
      3 seconds, //-> minimum wait time before the actor is restarted, on failure again the min time become 2x
      30 seconds, //-> maximum amount of time this actor will keep trying to restart before it stops
      0.2 //-> Randomness to the time, so that all actors does not start at same time
    ).withSupervisorStrategy(OneForOneStrategy(){ //-> Remember that default strategy for any actor is to restart, here we change that
      case _ => Stop //-> We have now specified that the Stop strategy should be the default on any error that occurs
    })
  )

//  val simpleStopSupervisor = system.actorOf(stopSupervisorProps, "stopSupervisor")
//  simpleStopSupervisor ! ReadFile

  //We now create a class that extends the FBPActor, but then try to read the file at initialization
  class EagerFBPActor extends FileBasedPersistenceActor {
    override def preStart(): Unit = {
      log.info("Eager actor starting")
      datasource = Source.fromFile(new File("src/main/resources/testfiles/important-data.txt"))
    }
  }

  //NB: When ever a child throws: ActorInitializationException, the default strategy is to STOP it,
  //which is why this actor will not start again when it fails with the above expected exception.
//  val eagerFBPActor = system.actorOf(Props[EagerFBPActor], "eagerFBPActor")

  //Now we create a BackoffPattern around this actor so that when it stops, we restart it at an exponential time.
  val repeatedSupervisorProps = BackoffSupervisor.props(
    Backoff.onStop(
      Props[EagerFBPActor],
      "eagerActor",
      1 second,
      30 seconds,
      0.1
    )
  )

  val repeatedSupervisor = system.actorOf(repeatedSupervisorProps, "repeatedBackoffSupervisor")
  /*
    This creates an actor `repeatedBackoffSupervisor`, and that actor creates a child `eagerActor`
    NB: ON start, the eagerActor will die, with ActorInitializationException, which causes our backoff plan to kick in
    Any message to repeatedBackoffSupervisor is passed on to eagerActor.
    Should eager actor stop, it will use the settings above and try to restart again,
     until it stops trying or gets the resource its wants.
     Hence if at some point the required resource becomes available between the time the actor is dying and restarting
      and dying again and restarting again, then the actor will start as normal without failing. This proves to us how
      we can supervise our actor that is dependent on an external resource like a DB, which is not always available
   */

}
