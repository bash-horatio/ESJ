import Boot.Init
import akka.actor._
import com.typesafe.config.ConfigFactory
import common.ConfHelper.ConfigHelper
import common.FileHelper.FileHelper
import common.FqueueHelper.FqueueHelper
import common.Log.LogLevel.Debug
import services.actor.SceneActor.PullFq
import services.actor._

import scala.collection.parallel.ParSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object ESJ extends App {

  val dynConfig = ConfigHelper.getConf()
  val queue = dynConfig.getString("Actor.Boot.Fqueue")
  val syncInterval = dynConfig.getString("Actor.Boot.SyncInterval").toInt
  val rulesFile = dynConfig.getString("Actor.Scene.Rules")
  val sceneIds = dynConfig.getString("Actor.Scene.Identities")
  val segSize = dynConfig.getString("Actor.Log.SegmentSize").toInt
  val sysName = dynConfig.getString("App.Name")

  val config = ConfigFactory.load()
//  implicit val system = ActorSystem(sysName, config.getConfig("AkkaConfig"))
  implicit val system = ActorSystem(sysName)
  val boot = system.actorOf(Props[Boot], "BootActor")

  boot ! Init
}


class Boot extends Actor with ActorLogging {

  val name = context.self.path.toString.split("/").last
  def receive = {
//    case Init => AsyncLogger.debug(Debug(self.getClass, "System booted"))
    case Init => println(Debug(self.getClass, "System booted"))
  }
}


class OldBoot extends Actor with ActorLogging {
  import Boot._
  import ESJ._
  import services.actor.LogActor.{Err, Info}
  import services.actor.SceneActor.LoadMap

  val name = context.self.path.toString.split("/").last
  val logActor = context.actorOf(Props(classOf[LogActor], segSize), "LogActor")
  context.watch(logActor)

  def receive = {
    case Init =>
      /***** reasonable to pass ActorRef as a parameter? *****/
      Future(ParSeq(
        (Props[PushActor], "PushActor"), (Props[RecommendActor], "RecommendActor"),
        (Props[SceneActor], "SceneActor")
      )) onComplete {
        case Success(props) =>
          val actors = props.map(prop =>
            context.watch(
              context.actorOf(prop._1, prop._2))
          ).toArray
          val sceneActor = actors.last
          sceneActor ! PullFq
          self ! SyncMap(sceneActor)

        case Failure(ex) => logActor ! Err(s"$name: Init: $ex")
      }

    case SyncMap(scene) =>
      //if (DateHelper.getCurrentHour.toInt % 8 == 0) { }
      val fqClient = FqueueHelper.client()
      Future(fqClient.pull(queue)) onComplete {
        case Success(map) =>
          if (map.nonEmpty) {
            FileHelper.save2File(rulesFile, map.get)
            logActor ! Info(s"$name: SyncMap: success to update rule map")
            scene ! LoadMap
          } else logActor ! Info(s"$name: SyncMap: no rule map to update")
        case Failure(ex) => logActor ! Err(s"$name: SyncMap: $ex")
      }

      Thread.sleep(syncInterval)
      self ! SyncMap(scene)

    case Shutdown =>
      val children = context.children
      if (children.isEmpty) {
        logActor ! Info(s"$name: Shutdown: all children already died before, ready to shutdown system")
        self ! PoisonPill
        context.system.shutdown()
      } else {
        children.par.foreach(child => child ! PoisonPill)
        logActor ! Info(s"$name: Shutdown: killed all children, ready to shutdown system")
        self ! PoisonPill
        context.system.shutdown()
      }
  }
}

object Boot {
  object Init
  case class SyncMap(scene: ActorRef)
  object Shutdown
}
