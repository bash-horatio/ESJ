package common.Async

import akka.actor._
import akka.event.Logging.{InitializeLogger, LoggerInitialized}
import common.Async.Logger._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NoStackTrace

/**
 * Created by horatio on 1/8/16.
 */
class LogActor extends Actor with AsyncLogger {
  override def receive: Receive = {
    case InitializeLogger(_) ⇒ sender() ! LoggerInitialized
    case event: LogEvent => println(event)
      apply(event)
  }
}


trait AsyncLogger {
  import java.text.SimpleDateFormat
  import java.util.Date

  private val date = new Date()
  private val dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss")
  private val logFormat = "[%s] [%s] [%s] %s"
  private val logFormatWithCause = "[%s] [%s] [%s] %s %s"

  private val errorBuffer = ArrayBuffer[String]()
  private val warningBuffer = ArrayBuffer[String]()
  private val infoBuffer = ArrayBuffer[String]()
  private val debugBuffer = ArrayBuffer[String]()

  private def simpleName(obj: AnyRef): String = simpleName(obj.getClass)
  private def  timestamp(event: LogEvent): String = synchronized {
    date.setTime(event.timestamp)
    dateFormat.format(date)
  }

  /**
   * Returns the StackTrace for the given Throwable as a String
   */
  private def stackTraceFor(e: Throwable): String = e match {
    case null ⇒ ""
    case _: NoStackTrace      ⇒ " (" + e.getClass.getName + ")"
    case other ⇒
      val sw = new java.io.StringWriter
      val pw = new java.io.PrintWriter(sw)
      pw.append('\n')
      other.printStackTrace(pw)
      sw.toString
  }

  private def write(log: String) = {

  }

  def apply(event: Any): Unit = event match {
    case e: Error   ⇒ error(e)
    case e: Warning ⇒ warning(e)
    case e: Info    ⇒ info(e)
    case e: Debug   ⇒ debug(e)
    case e          ⇒ warning(Warning(simpleName(this), this.getClass, "received unexpected event of class " + e.getClass + ": " + e))
  }

  def error(event: Error): Unit = {
    val form = if (event.cause == Error.NoCause) logFormat else logFormatWithCause
    println(form.format(
      timestamp(event),
      event.logClass,
      event.message,
      stackTraceFor(event.cause)))
  }

  def warning(event: Warning): Unit =
    println(logFormat.format(
      timestamp(event),
      event.logClass,
      event.message))

  def info(event: Info): Unit =
    println(logFormat.format(
      timestamp(event),
      event.logClass,
      event.message))

  def debug(event: Debug): Unit =
    println(logFormat.format(
      timestamp(event),
      event.logClass,
      event.message))
}