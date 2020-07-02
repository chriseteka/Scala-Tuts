package part2Actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2Actors.ChangingActorBehaviour.Mom.MomStart

object ChangingActorBehaviour extends App {

  object FussyKid {
    case object KidAccept
    case object KidReject
    val HAPPY = "Happy"
    val SAD = "Sad"
  }

  class FussyKid extends Actor {

    import FussyKid._
    import Mom._

    var state: String = HAPPY //Internal state of the kid
    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) =>
        if (state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor {
    import FussyKid._
    import Mom._

    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, discardOld = false)// Change my receive handler to sadReceive
      case Food(CHOCOLATE) =>
      case Ask(_) => sender() ! KidAccept
    }
    def sadReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, discardOld = false)
//      case Food(CHOCOLATE) => context.become(happyReceive, false)// Change my receive handler to happyReceive
      case Food(CHOCOLATE) => context.unbecome()
      case Ask(_) => sender() ! KidReject
    }
  }

  object Mom {
    case class MomStart(kidRef: ActorRef)
    case class Food(food: String)
    case class Ask(message: String)
    val VEGETABLE = "Veggies"
    val CHOCOLATE = "Chocolate"
  }
  class Mom extends Actor {

    import  Mom._
    import FussyKid._

    override def receive: Receive = {
      case MomStart(kidRef: ActorRef) =>
        //Testing kid by giving him vegetable
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("Do you want to play?")
      case KidAccept => println("Yay my kid is happy")
      case KidReject => println("My kid is sad, but he's healthy!")
    }
  }

  val system = ActorSystem("ChangingActorBehaviourDemo")
//  val fussyKid = system.actorOf(Props[FussyKid], "fussyKid")
//  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid], "statelessFussyKid")
//  val mom = system.actorOf(Props[Mom], "mom")

//  mom ! MomStart(statelessFussyKid)

  //When we use context.become(handler, true), we are totally replacing the old handler with a new one
  //When we use context.become(handler, false), we are keeping the new handler on the old one, in a stack form
  //true is the default arg when we just call context.become() with just a handler.
  //Since we push handlers to the stack when we use the false arg, we pop from this stack with context.unbecome()


  //EXERCISES

  object Counter {
    case object INCREMENT
    case object DECREMENT
    case object PRINT
    case class Data(var value: Int = 0)
  }

  class Counter extends Actor {
    import Counter._

    val finalData = Data()
    override def receive: Receive = PrintHandler(finalData)

    def IncrementHandler(data: Data): Receive = {
      case INCREMENT => data.value += 1
      case DECREMENT => context.become(DecrementHandler(data), discardOld = true)
      case PRINT => context.become(PrintHandler(data), discardOld = true)
    }

    def DecrementHandler(data: Data): Receive = {
      case INCREMENT => context.become(IncrementHandler(data), discardOld = true)
      case DECREMENT => Data(data.value - 1)
      case PRINT => context.become(PrintHandler(data), discardOld = true)
    }

    def PrintHandler(data: Data): Receive = {
      case INCREMENT => context.become(IncrementHandler(data), discardOld = true)
      case DECREMENT => context.become(DecrementHandler(data), discardOld = true)
      case PRINT => println(s"[Counter] Current Value: ${data.value}")
    }
  }

  object Communicator {
    case class BeginComm(ref: ActorRef)
  }
  class Communicator extends Actor {
    import Communicator._
    import Counter._

    override def receive: Receive = {
      case BeginComm(ref) =>
        ref ! PRINT
        (1 to 5).foreach(_ => ref ! INCREMENT)
        (1 to 3).foreach(_ => ref ! DECREMENT)
        ref ! PRINT
    }
  }

  import Communicator._

//  val communicator = system.actorOf(Props[Communicator], "communicator")
//  val counter = system.actorOf(Props[Counter], "counter")

//  communicator ! BeginComm(counter)

  //DOMAIN FOR THE VOTE SYSTEM
  object Citizen {
    val SUCCESSFUL_VOTING = "Successfully Voted"
    val FAILURE_VOTING = "Failed Voting"
    val ALREADY_VOTED = "Already Voted"
    val NO_CANDIDATE_VOTED = "Voted For No Candidate"

    case class Vote(candidate: String)
    case object VoteStatusRequest
    case object Print
    case class BeginVoting(data: Map[ActorRef, String], aggregator: ActorRef)
    case class VoteStatusReply(candidate: Option[String])
    case class AggregateVotes(citizens: Set[ActorRef])
    def props(name:String): Props = Props(new Citizen(name))
  }

  import Citizen._
  class Citizen(name: String) extends Actor {

    val citizenId: (ActorRef, String) = (self, name)

    override def receive: Receive = VoteReceive(Map())

    def VoteReceive(votes: Map[(ActorRef, String), String]): Receive = {
      case votingStatus: String => println(s"Vote Status: $name $votingStatus")
      case Vote(candidate) =>
        if (votes.nonEmpty && votes.keys.exists(_ equals citizenId)) self ! ALREADY_VOTED
        else{
          val updatedVotes = votes + (citizenId -> candidate)
          context.become(VoteReceive(updatedVotes))
          if (updatedVotes.nonEmpty && updatedVotes.keys.exists(_ equals citizenId)){
            if (candidate.isEmpty) self ! NO_CANDIDATE_VOTED
            else self ! SUCCESSFUL_VOTING
          }
          else self ! FAILURE_VOTING
        }
      case VoteStatusRequest => sender() ! VoteStatusReply(votes.get(citizenId))
    }
  }

  class VoteAggregator extends Actor {

    override def receive: Receive = aggregatorReceive(Map())

    def aggregatorReceive(result: Map[String, Int]): Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(_ ! VoteStatusRequest)
      case VoteStatusReply(candidate) =>
        if (candidate.nonEmpty) {
          val raw = candidate.get
          if (result.nonEmpty && result.keys.exists(_ equalsIgnoreCase raw)) {
            val oldVal = result - raw
            val updatedRes = raw -> (oldVal(raw) + 1)
            context.become(aggregatorReceive(result + updatedRes))
          }
          else context.become(aggregatorReceive(result + (raw -> 1)))
        }
        self ! Print
      case Print => result.foreach(println(_))
    }
  }

  class VoteSystem extends Actor {
    override def receive: Receive = {
      case BeginVoting(data, aggregator) =>
        data.foreach(d => d._1 ! Vote(d._2)) // Vote
        aggregator ! AggregateVotes(data.keys.toSet) // Aggregate
//        aggregator ! Print // Print Results
    }
  }

  // Citizens
//  val chris: ActorRef = system.actorOf(Citizen.props("Chris"), "chris")
//  val john: ActorRef = system.actorOf(Citizen.props("John"), "john")
//  val faith: ActorRef = system.actorOf(Citizen.props("Faith"), "faith")
//  val susan: ActorRef = system.actorOf(Citizen.props("Susan"), "susan")
//  val ann: ActorRef = system.actorOf(Citizen.props("Ann"), "ann")

  // Aggregator
//  val voteAggregator = system.actorOf(Props[VoteAggregator], "voteAggregator")

  // VoteSystem
//  val voteSystem = system.actorOf(Props[VoteSystem], "VoteSystem")

//  val voteCast = Map(chris -> "NIGERIA", john -> "KENYA", faith -> "KENYA", susan -> "RWANDA", ann -> "")
//  voteSystem ! BeginVoting(voteCast, voteAggregator)

  // EXERCISE 1 CORRECTIONS
  object CCount {
    case object INCREMENT
    case object DECREMENT
    case object PRINT
  }

  class CCount extends Actor {
    import CCount._

    override def receive: Receive = CountReceive(0)

    def CountReceive(currValue: Int): Receive = {
      case INCREMENT =>
        println(s"[countReceive($currValue)] incrementing")
        context.become(CountReceive(currValue + 1))
      case DECREMENT =>
        println(s"[countReceive($currValue)] decrementing")
        context.become(CountReceive(currValue - 1))
      case PRINT => println(s"[countReceive($currValue)] My current count is $currValue")
    }
  }

//  import CCount._
//  val correctedCounter = system.actorOf(Props[CCount], "correctedCounter")
//  correctedCounter ! PRINT
//  (1 to 5).foreach(_ => correctedCounter ! INCREMENT)
//  (1 to 3).foreach(_ => correctedCounter ! DECREMENT)
//  correctedCounter ! PRINT

  // EXERCISE 2 CORRECTION STYLE 1
//  case class CVote(candidate: String)
//  case object CVoteRequestStatus
//  case class CVoteStatusReply(candidate: Option[String])
//  class CCitizen extends Actor {
//    var candidate: Option[String] = None
//    override def receive: Receive = {
//      case CVote(c) => candidate = Some(c)
//      case CVoteRequestStatus => sender() ! CVoteStatusReply(candidate)
//    }
//  }
//
//  case class CAggregateVotes(citizen: Set[ActorRef])
//  class CVoteAggregator extends Actor {
//    var stillWaiting: Set[ActorRef] = Set()
//    var currentStats: Map[String, Int] = Map()
//
//    override def receive: Receive = {
//      case CAggregateVotes(citizens) =>
//        stillWaiting = citizens
//        citizens.foreach(_ ! CVoteRequestStatus)
//      case CVoteStatusReply(None) =>
//        sender() ! CVoteRequestStatus // May end in an infinite loop
//      case CVoteStatusReply(Some(candidate)) =>
//        val newStillWaiting = stillWaiting - sender()
//        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
//        currentStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
//        if (newStillWaiting.isEmpty)
//          println(s"[Aggregator] Pool stat: $currentStats")
//        else {
//          stillWaiting = newStillWaiting
//        }
//    }
//  }

  // EXERCISE 2 CORRECTION STYLE 2
  case class CVote(candidate: String)
  case object CVoteRequestStatus
  case class CVoteStatusReply(candidate: Option[String])
  class CCitizen extends Actor {
    override def receive: Receive = {
      case CVote(c) => context.become(voted(c))
      case CVoteRequestStatus => sender() ! CVoteStatusReply(None)
    }

    def voted(candidate: String): Receive = {
      case CVoteRequestStatus => sender() ! CVoteStatusReply(Some(candidate))
    }
  }

  case class CAggregateVotes(citizen: Set[ActorRef])
  class CVoteAggregator extends Actor {

    override def receive: Receive = awaitingCommand

    def awaitingCommand: Receive = {
      case CAggregateVotes(citizens) =>
        citizens.foreach(_ ! CVoteRequestStatus)
        context.become(awaitingStatuses(citizens, Map()))
    }

    def awaitingStatuses(stillWaiting: Set[ActorRef], currentStats: Map[String, Int]): Receive = {
      case CVoteStatusReply(None) =>
        sender() ! CVoteRequestStatus // May end in an infinite loop
      case CVoteStatusReply(Some(candidate)) =>
        val newStillWaiting = stillWaiting - sender()
        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
        val updatedStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
        if (newStillWaiting.isEmpty)
          println(s"[Aggregator] Pool stat: $updatedStats")
        else {
          //We still need to process result from other citizens
          context.become(awaitingStatuses(newStillWaiting, updatedStats))
        }
    }
  }

  val chris: ActorRef = system.actorOf(Props[CCitizen], "chris")
  val john: ActorRef = system.actorOf(Props[CCitizen], "john")
  val faith: ActorRef = system.actorOf(Props[CCitizen], "faith")
  val susan: ActorRef = system.actorOf(Props[CCitizen], "susan")
  val ann: ActorRef = system.actorOf(Props[CCitizen], "ann")

  chris ! CVote("NIGERIA")
  john ! CVote("CONGO")
  faith ! CVote("CONGO")
  susan ! CVote("MOON")
  ann ! CVote("STAR")

  val agg = system.actorOf(Props[CVoteAggregator], "agg")
  agg ! CAggregateVotes(Set(chris, john, faith, susan, ann))
}
