package common.Log

import akka.actor.{ActorSystem, NoSerializationVerificationNeeded, Props}
import common.Log.LogLevel._

import scala.language.existentials
import scala.util.control.NoStackTrace

/**
 * Created by horatio on 1/8/16.
 */
object AsyncLogger {

  protected implicit val system = ActorSystem("AsyncLogger")
  val logger = system.actorOf(Props[LogActor])

  def debug(event: Debug) = logger ! event
  def info(event: Info) = logger ! event
  def warning(event: Warning) = logger ! event
  def error(event: Error) = logger ! event
}


/**
 * Base type of LogEvents
 */
sealed trait LogEvent extends NoSerializationVerificationNeeded {
  /**
   * When this LogEvent was created according to System.currentTimeMillis
   */
  val timestamp: Long = System.currentTimeMillis

  /**
   * The LogLevel of this LogEvent
   */
  def level: String

  /**
   * The class of the source of this event
   */
  def logClass: Class[_]

  /**
   * The message, may be any object or null.
   */
  def message: Any

  /**
   * flag about flash Log Buffer to disk
   */
  def flashFlag: Boolean = false
}


object LogLevel {
  /** Null Object used for errors without cause Throwable */
  object NoCause extends NoStackTrace

  /**
   * For ERROR Logging
   */
  case class Error(cause: Throwable, logClass: Class[_], message: Any = "") extends LogEvent {
    def this(logClass: Class[_], message: Any) = this(NoCause, logClass, message)
    override def level = "Error"
  }

  /**
   * For WARNING Logging
   */
  case class Warning(logClass: Class[_], message: Any = "") extends LogEvent {
    override def level = "Warning"
  }

  /**
   * For INFO Logging
   */
  case class Info(logClass: Class[_], message: Any = "") extends LogEvent {
    override def level = "Info"
  }

  /**
   * For DEBUG Logging
   */
  case class Debug(logClass: Class[_], message: Any = "") extends LogEvent {
    override def level = "Debug"
  }

  case class Flash(logClass: Class[_], message: Any = "") extends LogEvent {
    override def level = "Flash"
    override def flashFlag = true
  }
}