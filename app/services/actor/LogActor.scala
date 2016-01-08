package services.actor

import akka.actor.{Actor, ActorLogging}
import akka.event.Logging._
import common.LogHelper.LogHelper
import services.actor.LogActor.{Err, Info, Warn}

import scala.collection.mutable.ListBuffer

/**
 * Created by horatio on 10/28/15.
 */


class LogActor extends Actor {
  def receive = {
    case InitializeLogger(_)                        => sender() ! LoggerInitialized
    case Error(cause, logSource, logClass, message) =>
    case Warning(logSource, logClass, message)      =>
    case Debug(logSource, logClass, message)        =>
  }
}


class OldLogActor(size: Int) extends Actor with ActorLogging {

  val name = context.self.path.toString.split("/").last
  val errBuffer = ListBuffer[String]()
  val warnBuffer = ListBuffer[String]()
  val infoBuffer = ListBuffer[String]()

  def receive = {
    case Err(msgs) =>
      errBuffer += msgs
      log.info(s"$name: Err: buffer size: $size, now: ${errBuffer.length}")
      log.error(msgs)

      if (errBuffer.length >= size) {
          errBuffer.foreach(log => LogHelper.err(log))
          errBuffer.clear
      }

    case Warn(msgs) =>
      warnBuffer += msgs
      log.info(s"$name: Warn: buffer size: $size, now: ${warnBuffer.length}")
      log.warning(msgs)

      if (warnBuffer.length >= size) {
        warnBuffer.foreach(log => LogHelper.warn(log))
        warnBuffer.clear
      }

    case Info(msgs) =>
      infoBuffer += msgs
      log.info(s"$name: Info: buffer size: $size, now: ${infoBuffer.length}")
      log.info(msgs)

      if (infoBuffer.length >= size) {
        infoBuffer.foreach(log => LogHelper.info(log))
        infoBuffer.clear
      }
  }
}

object LogActor {
  case class Err(msgs: String)
  case class Warn(msgs: String)
  case class Info(msgs: String)
  case class Debug(msgs: String)
  case class Write(buf: ListBuffer[String])
}