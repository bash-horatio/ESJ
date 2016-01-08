package common

import akka.actor.ActorSystem

import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import common.Async.LogActor
import common.Async.Logger.Debug
import org.scalatest.{MustMatchers, WordSpecLike}

/**
 * Created by horatio on 1/8/16.
 */
class LogActorTest  extends TestKit(ActorSystem("AsyncLogger")) with ImplicitSender with WordSpecLike with MustMatchers {
    "LogActor" must {
      "print a log msg" in {
        val log = TestActorRef[LogActor]
        val name = log.path.toString.split("/").last
        log ! Debug(name, log.getClass, "Debug LogActor")
      }
    }
}
