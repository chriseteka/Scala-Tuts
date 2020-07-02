package part2Actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object IntroAkkaConfig extends App {

  class SimpleLoggingActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  /**
    * 1. Inline config: a way to pass config to an actor system
    */
  val configString =
    """
      |akka {
      | loglevel = "ERROR"
      |}
    """.stripMargin

  /**
    * 2. Config in a file, this is common, usually in the "/src/main/resources" folder, and named "application.conf"
    */

  val config = ConfigFactory.parseString(configString)
  //Now when creating an actor system, we can load the config created as thus:
  val system = ActorSystem("AkkaConfigDemo", ConfigFactory.load(config))
  val actor = system.actorOf(Props[SimpleLoggingActor])
  actor ! "A message to remember"

  // By Default, src/main/resources/application.conf is loaded
  val defaultConfigFileSystem = ActorSystem("DefaultConfigFileDemo")
  val actor2 = defaultConfigFileSystem.actorOf(Props[SimpleLoggingActor])
  actor2 ! "Remember me"

  /**
    * 3. Separate config in same file: Here all config for various actors is in same file (app.conf)
    * but their individual config (config per actor) is separated by name spaces
    */
  val specialConfig = ConfigFactory.load().getConfig("mySpecialConfig")
  val specialConfigSystem = ActorSystem("specialConfigSystem", specialConfig)
  val specialConfigActor = specialConfigSystem.actorOf(Props[SimpleLoggingActor])
  specialConfigActor ! "Remember me, I am special"

  /**
    * 4. Separate config in different files: Here every config is specified in their own individual file
    */
  val separateConfig = ConfigFactory.load("secret/SecretConfiguration.conf")
  println(s"Seperate config log level ${separateConfig.getString("akka.loglevel")}")

  /**
    * NOW LETS CONSIDER THE VARIOUS FORMATS THAT AKKA TAKES IN CONFIG
    * 1 .conf
    * 2 .json
    * 3 .properties
    */
  val jsonConfig = ConfigFactory.load("json/jsonConfig.json")
  println(s"A json value from json config file ${jsonConfig.getString("aJsonProperty")}")
  println(s"Akka loglevel from json config file ${jsonConfig.getString("akka.loglevel")}")

  val propConfig = ConfigFactory.load("props/propsConfig.properties")
  println(s"A value from props config file ${propConfig.getString("my.simpleProperty")}")
  println(s"Akka loglevel from props config file ${propConfig.getString("akka.loglevel")}")
}
