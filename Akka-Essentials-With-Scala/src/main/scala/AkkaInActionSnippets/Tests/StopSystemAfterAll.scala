package AkkaInActionSnippets.Tests

import akka.testkit.TestKit
import org.scalatest.{BeforeAndAfterAll, Suite}

trait StopSystemAfterAll extends BeforeAndAfterAll {
  this: TestKit with Suite =>

  override protected def afterAll() {
    super.afterAll()
    TestKit.shutdownActorSystem(system)
  }
}
