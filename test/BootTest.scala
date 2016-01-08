import Boot.Init
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import org.scalatest.MustMatchers
import org.scalatest.WordSpecLike

/**
 * Created by horatio on 1/8/16.
 */
class BootTest extends TestKit(ActorSystem("MyActorSystem")) with ImplicitSender with WordSpecLike with MustMatchers {
  "Boot" must {
    "print a initializing msg" in {
      val boot = TestActorRef[Boot]
      boot ! Init
    }
  }
}
