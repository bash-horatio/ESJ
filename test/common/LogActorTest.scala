package common

import akka.actor.ActorSystem

import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import common.Log.LogActor
import common.Log.LogLevel.Debug

import org.scalatest.{MustMatchers, WordSpecLike}
import scala.language.implicitConversions

/**
 * Created by horatio on 1/8/16.
 */
class LogActorTest  extends TestKit(ActorSystem("AsyncLogger")) with ImplicitSender with WordSpecLike with MustMatchers {
  "LogActor" must {
    "print a log msg" in {
      val log = TestActorRef[LogActor]
      val name = log.path.toString.split("/").last
      for (i <- 1 to 7)
      log ! Debug(log.getClass, "Debug LogActor")
    }
  }
}
