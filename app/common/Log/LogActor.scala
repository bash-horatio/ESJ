package common.Log

import akka.actor.Actor
import akka.event.Logging.{InitializeLogger, LoggerInitialized}
import common.File.LocalFileTool
import common.Log.LogLevel.{Debug, Error, Info, Warning}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NoStackTrace

/**
 * Created by horatio on 1/8/16.
 */
class LogActor extends Actor with Logger {
  override def receive: Receive = {
    case InitializeLogger(_) => sender() ! LoggerInitialized
    case event: LogEvent => write(event)
  }
}

trait Logger {
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
    case _: NoStackTrace ⇒ " (" + e.getClass.getName + ")"
    case other ⇒
      val sw = new java.io.StringWriter
      val pw = new java.io.PrintWriter(sw)
      pw.append('\n')
      other.printStackTrace(pw)
      sw.toString
  }

  /**
   * apply to add Log Event to Buffer or flash Buffer to disk
   */
  private def apply(logBuffer: ArrayBuffer[String], event: LogEvent) = {
    import common.Log.LogLevel.NoCause
    val threshold = 16
    val errorLogFile = "./logs/err.log"

    event.level match {
      case "Error" =>
        val e = event.asInstanceOf[Error]
        val form = if (e.cause == NoCause) logFormat else logFormatWithCause
        val logMessage = form.format(timestamp(event), event.logClass,
          event.message, stackTraceFor(e.cause))
        logBuffer += logMessage

        if((logBuffer.size) >= threshold) || (event.flashFlag == true) {
          val append = true
          LocalFileTool.append(errorLogFile, logBuffer.foldRight("\n")(_ + _))
    }

      case _ =>
        val logMessage = logFormat.format(timestamp(event), event.logClass, event.message)
    }

  }

  def write(event: LogEvent): Unit = event match {
    case e: Error   ⇒ this.apply(errorBuffer, e)
    case e: Warning ⇒ this.apply(warningBuffer, e)
    case e: Info    ⇒ this.apply(infoBuffer, e)
    case e: Debug   ⇒ this.apply(debugBuffer, e)
    case e          ⇒ this.write(Warning(this.getClass, s"received unexpected event of class: $e"))
  }
}