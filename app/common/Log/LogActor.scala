package common.Log

import java.io.{BufferedOutputStream, FileOutputStream}

import akka.actor._
import common.ConfHelper.ConfigHelper
import common.Log.LogLevel._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * Log Actor to receive log events.
 * Capable to transform into Akka Event Stream Listener
 *
 */
class LogActor extends Actor with Logger {
  import akka.event.Logging.{InitializeLogger, LoggerInitialized}

  import scala.language.postfixOps

  /**
   * create a schedule to flush and clear regularly before receiving message
   */
  override def preStart() = {
    val dynConfig = ConfigHelper.getConf()
    val flushInterval = dynConfig.getString("Log.Actor.FlushInterval").toInt
    val startDelay = dynConfig.getString("Log.Actor.StartDelay").toInt
    context.system.scheduler.schedule(startDelay minutes, flushInterval seconds, self, Flush(self.getClass, "flush to File and clear Log Buffer regularly"))
  }

  /**
   * override de default supervisor strategy to send Error message
   */
//  override val supervisorStrategy=OneForOneStrategy() {
//    case ex: ActorInitializationException =>
//      classify(Error(self.getClass, "failed to init", ex))
//      Stop
//    case ex: ActorKilledException =>
//      classify(Error(self.getClass, "be killed", ex))
//      Stop
//    case ex: DeathPactException =>
//      classify(Error(self.getClass, "death pact", ex))
//      Stop
//    case ex: Exception =>
//      classify(Error(self.getClass, "exception", ex))
//      Restart
//  }

  override def receive: Actor.Receive = {
    case event: LogEvent => classify(event)

    /**
     * response to InitializeLogger(_) when registering this LogActor to EventStream Listener
     */
    case InitializeLogger(_) => sender() ! LoggerInitialized
  }
}

trait Logger {
  import Logger._

  /**
   * flush Log Buffer to File
   */
  private def flushToFile(logFile: String, bufferContent: ArrayBuffer[String]) = {
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
      case ex: Exception => this.classify(Error(this.getClass, "failed", ex))
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

      case e: Flush =>
        val logMessage = logFormat.format(e.level, formatTime(e), event.logClass,
        e.message)
        errorBuffer += logMessage + "\n"
    }

    if (event.flushFlag || errorBuffer.size >= lowThreshold) {
      flushToFile(errorLogFile, errorBuffer)
      errorBuffer.clear()
    }
  }

  private def warning(event: LogEvent) = {
    val logContent = logFormat.format(event.level, formatTime(event), event.logClass, event.message)
    warningBuffer += logContent + "\n"
    if (event.flushFlag || warningBuffer.size >= lowThreshold) {
      flushToFile(warningLogFile, warningBuffer)
      warningBuffer.clear()
    }
  }

  private def info(event: LogEvent) = {
    val logContent = logFormat.format(event.level, formatTime(event), event.logClass, event.message)
    infoBuffer += logContent + "\n"
    if (event.flushFlag || infoBuffer.size >= highThreshold) {
      flushToFile(infoLogFile, infoBuffer)
      infoBuffer.clear()
    }

  }

  private def debug(event: LogEvent) = {
    val logContent = logFormat.format(event.level, formatTime(event), event.logClass, event.message)
    debugBuffer += logContent + "\n"
    if (event.flushFlag || debugBuffer.size >= highThreshold) {
      flushToFile(debugLogFile, debugBuffer)
      debugBuffer.clear()
    }

  }

  private def flush(event: LogEvent): Unit = {
    val flush = Flush(event.logClass, event.message)
    this.error(flush)
    this.warning(flush)
    this.info(flush)
    this.debug(flush)
  }

  def classify(event: LogEvent): Unit = event match {
    case e: Flush => this.flush(e)
    case e: Error => this.error(e)
    case e: Warning => this.warning(e)
    case e: Info => this.info(e)
    case e: Debug => this.debug(e)
    case e => this.classify(Warning(this.getClass, s"received unexpected event of class: $e"))
  }

}


object Logger {
  import java.text.SimpleDateFormat
  import java.util.Date

  import scala.util.control.NoStackTrace

  /**
   * different formats of Log Content, only Error level has Cause(stack trace)
   */
  private val date = new Date()
  private val logFormat = "[%s] [%s] [%s] %s"
  private val logFormatWithCause = "[%s] [%s] [%s] %s %s"
  
  /**
   * different formats of system time bug SimpleDateFormat library isn't threadsafe
   * @param event
   * @return
   */
  private def formatTime(event: LogEvent): String = synchronized {
    date.setTime(event.timestamp)
    val dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss")
    dateFormat.format(date)
  }

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

  /**
   * get Log File name from ./DynConfig/dynamic.conf
   */
  private val dynConfig = ConfigHelper.getConf()
  private val logFileDir = dynConfig.getString("Log.File.Directory")
  private val logTimeStamp = formatTime(System.currentTimeMillis)
  private val errorLogFile = s"$logFileDir/error-$logTimeStamp"
  private val warningLogFile = s"$logFileDir/warning-$logTimeStamp"
  private val infoLogFile = s"$logFileDir/info-$logTimeStamp"
  private val debugLogFile = s"$logFileDir/debug-$logTimeStamp"

  /**
   * Log Buffer initialization
   */
  private val highThreshold = dynConfig.getString("Log.Buffer.HighThreshold").toInt
  private val lowThreshold = dynConfig.getString("Log.Buffer.LowThreshold").toInt
  private val errorBuffer = ArrayBuffer[String]()
  private val warningBuffer = ArrayBuffer[String]()
  private val infoBuffer = ArrayBuffer[String]()
  private val debugBuffer = ArrayBuffer[String]()
}