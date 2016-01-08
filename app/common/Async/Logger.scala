package common.Async

import akka.actor.{ActorSystem, NoSerializationVerificationNeeded, Props}

import scala.language.existentials
import scala.util.control.NoStackTrace

/**
 * Created by horatio on 1/8/16.
 */
object Logger {

  type MDC = Map[String, Any]
  val emptyMDC: MDC = Map()
  /**
   * Base type of LogEvents
   */
  sealed trait LogEvent extends NoSerializationVerificationNeeded {
    /**
     * The thread that created this log event
     */
    @transient
    val thread: Thread = Thread.currentThread

    /**
     * When this LogEvent was created according to System.currentTimeMillis
     */
    val timestamp: Long = System.currentTimeMillis

    /**
     * The source of this event
     */
    def logSource: String

    /**
     * The class of the source of this event
     */
    def logClass: Class[_]

    /**
     * The message, may be any object or null.
     */
    def message: Any

    /**
     * Extra values for adding to MDC
     */
    def mdc: MDC = emptyMDC
  }

  /**
   * For ERROR Logging
   */
  case class Error(cause: Throwable, logSource: String, logClass: Class[_], message: Any = "") extends LogEvent {
    def this(logSource: String, logClass: Class[_], message: Any) = this(Error.NoCause, logSource, logClass, message)
  }
  class Error2(cause: Throwable, logSource: String, logClass: Class[_], message: Any = "", override val mdc: MDC) extends Error(cause, logSource, logClass, message) {
    def this(logSource: String, logClass: Class[_], message: Any, mdc: MDC) = this(Error.NoCause, logSource, logClass, message, mdc)
  }

  object Error {
    def apply(logSource: String, logClass: Class[_], message: Any) = new Error(NoCause, logSource, logClass, message)
    def apply(cause: Throwable, logSource: String, logClass: Class[_], message: Any, mdc: MDC) = new Error2(cause, logSource, logClass, message, mdc)
    def apply(logSource: String, logClass: Class[_], message: Any, mdc: MDC) = new Error2(NoCause, logSource, logClass, message, mdc)

    /** Null Object used for errors without cause Throwable */
    object NoCause extends NoStackTrace
  }
  def noCause = Error.NoCause

  /**
   * For WARNING Logging
   */
  case class Warning(logSource: String, logClass: Class[_], message: Any = "") extends LogEvent
  class Warning2(logSource: String, logClass: Class[_], message: Any, override val mdc: MDC) extends Warning(logSource, logClass, message)
  object Warning {
    def apply(logSource: String, logClass: Class[_], message: Any, mdc: MDC) = new Warning2(logSource, logClass, message, mdc)
  }

  /**
   * For INFO Logging
   */
  case class Info(logSource: String, logClass: Class[_], message: Any = "") extends LogEvent
  class Info2(logSource: String, logClass: Class[_], message: Any, override val mdc: MDC) extends Info(logSource, logClass, message)
  object Info {
    def apply(logSource: String, logClass: Class[_], message: Any, mdc: MDC) = new Info2(logSource, logClass, message, mdc)
  }

  /**
   * For DEBUG Logging
   */
  case class Debug(logSource: String, logClass: Class[_], message: Any = "") extends LogEvent
  class Debug2(logSource: String, logClass: Class[_], message: Any, override val mdc: MDC) extends Debug(logSource, logClass, message)
  object Debug {
    def apply(logSource: String, logClass: Class[_], message: Any, mdc: MDC) = new Debug2(logSource, logClass, message, mdc)
  }

  protected implicit val system = ActorSystem("AsyncLogger")
  val logger = system.actorOf(Props[LogActor])

  def debug(e: Debug) = logger ! e
  def info(e: Info) = logger ! e
  def warning(e: Warning) = logger ! e
  def error(e: Error) = logger ! e
}

