package common.Log

import akka.actor.{ActorSystem, NoSerializationVerificationNeeded, Props}
import common.Log.LogLevel._

import scala.language.existentials

/**
 * Copyright (c) 2016/01/09, BoDao Tech, Inc. All Rights Reserved.
 * Author @ bash.horatio@gmai.com
 * Async Log System using Akka Actor Model
 * flushToFile: flush to File and clear Log Buffer
 * error, warning, info, debug: send different log level events to Log Buffer
 */
object AsyncLogger {
  protected implicit val system = ActorSystem("AsyncLogger")
  private val logger = system.actorOf(Props[LogActor])

  def flushToFile = logger ! Flush(system.getClass, "flush and clear immediately")
  def error(logClass: Class[_], message: String, cause: Throwable) = logger ! Error(logClass, message, cause)
  def error(logClass: Class[_], message: String) = logger ! new Error(logClass, message)
  def warning(logClass: Class[_], message: String) = logger ! Warning(logClass, message)
  def info(logClass: Class[_], message: String) = logger ! Info(logClass, message)
  def debug(logClass: Class[_], message: String) = logger ! Debug(logClass, message)
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
  def flushFlag: Boolean = false
}


object LogLevel {
  import scala.util.control.NoStackTrace
  /** Null Object used for errors without cause Throwable */
  object NoCause extends NoStackTrace

  /**
   * For ERROR Logging
   */
  case class Error(logClass: Class[_], message: Any = "", cause: Throwable) extends LogEvent {
    def this(logClass: Class[_], message: Any) = this(logClass, message, NoCause)
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

  case class Flush(logClass: Class[_], message: Any = "") extends LogEvent {
    override def level = "Flush"
    override def flushFlag = true
  }
}