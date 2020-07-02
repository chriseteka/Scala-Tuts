package part2Actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

//Distributed Word Counting Exercise
object ChildActorsExercise extends App {

  //Master and Worker will use this Companion object
  object WordCounterMaster {

    /* FLOW:
      1. WordCounterMaster Receives an Initialize(n: Int) message, and creates n number of WordCounterWorkers
      2. WordCounterMaster Receives a String (the word to count), then it responds by sending
       WordCountTask(message: String) to any of the WordCountWorkers
      3. WordCountWorker receives WordCountTask(message: String) and responds to it by sending WordCountReply(count:Int)
      4. WordCounterMaster receives this reply and then reply back to the sender with the actual result

      You have been asked to use round robin alg (after w1, next w2, ...) to specify which worker receives
       the next message as they are sent. Try also to add extra info to WordCountTask and WordCountReply
     */

    case class Initialize(nChildren: Int) // As response: create n-children of type WordCounterWorker
    case class WordCountTask(messageId: Int, text: String) //As request: Received by the word counter worker
    case class WordCountReply(messageId: Int, count: Int)
  }

  //Master
  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    // Set(Actor-1 ref, Actor-2 ref, ... ref, Actor-nChildren ref)
    def createWorkers(n: Int): Seq[ActorRef] =
      for (i <- 1 to n) yield context.actorOf(Props[WordCounterWorker], s"Actor-$i")
//      (1 to n).map(child => context.actorOf(Props[WordCounterWorker], s"Actor-$child"))

    override def receive: Receive = {
      case Initialize(nChildren) =>
        println("master initializing children")
        context.become(communicateWithChild(createWorkers(nChildren)))
    }

    def communicateWithChild(children: Seq[ActorRef], messageId: Int = 0, requestMap: Map[Int, ActorRef] = Map()): Receive = {
      case message: String =>
        val nextMessageTo = children.head
        val newRequestMap = requestMap + (messageId -> sender())
        println(s"[${self.path.name}] Received message: $message, now sending it to worker: [${nextMessageTo.path.name}]")
        nextMessageTo ! WordCountTask(messageId, message)
        context.become(communicateWithChild(children.tail :+ nextMessageTo, messageId + 1, newRequestMap))
      case WordCountReply(id, count) =>
        println(s"[${sender().path.name}] counted message with id [$id], count:= $count")
        requestMap(id) ! count
        context.become(communicateWithChild(children, messageId, requestMap - id))
    }
  }

  //Worker
  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(messageId, text) =>
        println(s"[${self.path.name}] Received message: $text, now counting it.")
        sender() ! WordCountReply(messageId, text.split(" ").length)
    }
  }

  class TestActor extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster], "WordCountMaster")
        master ! Initialize(3)
        val texts = List("I love akka", "Scala is dope", "Yes", "me too")
        texts.foreach(master ! _)
      case count: Int =>
        println(s"[Test Actor] Received a reply: $count")

    }
  }

  //System Actor
  val system = ActorSystem("ChildActorsExerciseSystem")
  val testActor = system.actorOf(Props[TestActor], "TestActor")
  testActor ! "go"

}
