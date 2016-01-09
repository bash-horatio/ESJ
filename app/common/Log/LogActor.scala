package common.Log

import java.io.{BufferedOutputStream, FileOutputStream, FileWriter

import akka.event.Logging.{InitializeLogger, LoggerInitialized}
import akka.actor.Actor
import common.ConfHelper.ConfigHelper
import common.Log.LogLevel._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.control.NoStackTrace

/**
 * Created by horatio on 1/8/16.
 */
class LogActor extends Actor with Logger {

  override def preStart() = {
    val dynConfig = ConfigHelper.getConf()
    val flashInterval = dynConfig.getString("Log.Actor.FlashInterval").toInt
    val startDelay = dynConfig.getString("Log.Actor.StartDelay").toInt
    context.system.scheduler.schedule(startDelay minutes, flashInterval seconds, self, Flash(self.getClass, "flash to File and clear Log Buffer regularly"))
  }

  override def receive: Receive = {
    case InitializeLogger(_) => sender() ! LoggerInitialized
    case event: LogEvent => apply(event)
  }
}

trait Logger {
  import Logger._
  /**
   * flash Log Buffer to File
   */
  private def flashToFile(logFile: String, bufferContent: String) = {
    var out: FileWriter = null
    var status = false
    try {
      out = new FileWriter(logFile, true)
      out.write(bufferContent)
    } catch {
      case ex: Exception =>

        status = false
    } finally {
      if (out != null) out.close()
    }
  }

  private def flashToFile(logFile: String, bufferContent: ArrayBuffer[String]) = {
    val append = true
    var fos: FileOutputStream = null
    var bos: BufferedOutputStream = null
    try {
      fos = new FileOutputStream(logFile, append)
      bos = new BufferedOutputStream(fos)
      bufferContent foreach { content =>
        bos.write(content.getBytes())
      }
      bos.flush()
    } catch {
      case ex: Exception => this.apply(Error(ex, this.getClass, "failed"))
    } finally {
      if (bos != null) bos.close()
      if (fos != null) fos.close()
    }

  }

  private def error(event: LogEvent) = {
    event match {
      case e: Error =>
        val form = if (e.cause == NoCause) logFormat else logFormatWithCause
        val logMessage = form.format(e.level, formatTime(e), event.logClass,
          e.message, stackTraceFor(e.cause))
        errorBuffer += logMessage + "\n"

      case e: Flash =>
        val logMessage = logFormat.format(e.level, formatTime(e), event.logClass,
        e.message)
        errorBuffer += logMessage + "\n"
    }

    if (event.flashFlag || errorBuffer.size >= lowThreshold) {
      flashToFile(errorLogFile, errorBuffer)
      errorBuffer.clear()
    }
  }

  private def warning(event: LogEvent) = {
    val logContent = logFormat.format(event.level, formatTime(event), event.logClass, event.message)
    warningBuffer += logContent + "\n"
    if (event.flashFlag || warningBuffer.size >= lowThreshold) {
      flashToFile(warningLogFile, warningBuffer)
      warningBuffer.clear()
    }
  }

  private def info(event: LogEvent) = {
    val logContent = logFormat.format(event.level, formatTime(event), event.logClass, event.message)
    infoBuffer += logContent + "\n"
    if (event.flashFlag || infoBuffer.size >= highThreshold) {
      flashToFile(infoLogFile, infoBuffer)
      infoBuffer.clear()
    }

  }

  private def debug(event: LogEvent) = {
    val logContent = logFormat.format(event.level, formatTime(event), event.logClass, event.message)
    debugBuffer += logContent + "\n"
    if (event.flashFlag || debugBuffer.size >= highThreshold) {
      flashToFile(debugLogFile, debugBuffer)
      debugBuffer.clear()
    }

  }

  private def flash(event: LogEvent): Unit = {
    val flash = Flash(event.logClass, event.message)
    this.error(flash)
    this.warning(flash)
    this.info(flash)
    this.debug(flash)
  }

  def apply(event: LogEvent): Unit = event match {
    case e: Flash => this.flash(e)
    case e: Error => this.error(e)
    case e: Warning => this.warning(e)
    case e: Info => this.info(e)
    case e: Debug => this.debug(e)
    case e => this.apply(Warning(this.getClass, s"received unexpected event of class: $e"))
  }

}


object Logger {
  import java.text.SimpleDateFormat
  import java.util.Date

  private val date = new Date()
  private val dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss")
  private val logFormat = "[%s] [%s] [%s] %s"
  private val logFormatWithCause = "[%s] [%s] [%s] %s %s"

  private def formatTime(event: LogEvent): String = synchronized {
    date.setTime(event.timestamp)
    dateFormat.format(date)
  } // SDF isn't threadsafe

  private def formatTime(timestamp: Long): String = synchronized {
    date.setTime(timestamp)
    val dateFormat = new SimpleDateFormat("yyyyMMdd")
    dateFormat.format(date)
  }

  /**
   * Returns the StackTrace for the given Throwable as a String
   */
  private def stackTraceFor(e: Throwable): String = e match {
    case null => ""
    case _: NoStackTrace => " (" + e.getClass.getName + ")"
    case other =>
      val sw = new java.io.StringWriter
      val pw = new java.io.PrintWriter(sw)
      pw.append('\n')
      other.printStackTrace(pw)
      sw.toString
  }


  val dynConfig = ConfigHelper.getConf()
  private val logFileDir = dynConfig.getString("Log.File.Directory")
  private val logTimeStamp = formatTime(System.currentTimeMillis)
  private val errorLogFile = s"$logFileDir/error-$logTimeStamp"
  private val warningLogFile = s"$logFileDir/warning-$logTimeStamp"
  private val infoLogFile = s"$logFileDir/info-$logTimeStamp"
  private val debugLogFile = s"$logFileDir/debug-$logTimeStamp"

  private val highThreshold = dynConfig.getString("Log.Buffer.HighThreshold").toInt
  private val lowThreshold = dynConfig.getString("Log.Buffer.LowThreshold").toInt
  private val errorBuffer = ArrayBuffer[String]()
  private val warningBuffer = ArrayBuffer[String]()
  private val infoBuffer = ArrayBuffer[String]()
  private val debugBuffer = ArrayBuffer[String]()
}