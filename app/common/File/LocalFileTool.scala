package common.File

import java.io._

import scala.collection.TraversableOnce
import scala.collection.mutable.{ArrayBuffer, Map => MuMap}
import scala.io.{BufferedSource, Source}


object LocalFileTool {
  /**
   * 获取文件的大小
   * 当文件不存在或者fileName不是一个规则文件时，返回-1
   * @param fileName 文件名字
   * @return
   */
  def fileSize(fileName: String): Long = {
    val file = new File(fileName)
    if (file.exists() && file.isFile) {
      file.length()
    } else {
      -1
    }
  }

  /**
   * 获取当前进程的工作路径
   * @return
   */
  def pwd(): String = {
    val dir = new File("")
    dir.getAbsolutePath
  }

  /**
   * 异常的时候会将错误保存到//BDCError错误队列里
   * @param fileName 文件名字
   * @return
   */
  def clean(fileName: String): Boolean = {
    var fileWriter: FileWriter = null
    var status = false

    try {
      val file = new File(fileName)
      if (!file.exists()) {
        //BDCError.logError(s"$fileName is not found")
        status = false
      } else {
        fileWriter = new FileWriter(file)
        fileWriter.write("")  //写入空
        status = true
      }
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        status = false
    } finally {
      if (fileWriter != null) fileWriter.close()
    }

    status
  }

  /**
   *
   * @param fileName 文件名字
   * @return
   */
  def exists(fileName: String): Boolean = {
    val file = new File(fileName)
    if (file.exists()) true else false
  }

  /**
   * 获取文件的所有内容，以行的形式存储在Array中
   * 发生异常的时候返回None
   * @param fileName 文件名字
   * @return
   */
  def getFileLines(fileName: String): Option[Array[String]] = {
    var source: BufferedSource = null
    var ret: Option[Array[String]] = None

    try {
      source = Source.fromFile(fileName)
      val lines = source.getLines()
      ret = Some(lines.toArray)
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        ret = None
    } finally {
      if (source != null) source.close()
    }

    ret
  }

  /**
   * 获取文件的内容，以行的形式返回
   * 文件内容行的首尾空格会被去掉，尾部的'\n'会被去掉
   * 如果发生异常，返回None
   * @param fileName 文件名字
   * @return
   */
  def getFileLinesTrim(fileName: String): Option[Array[String]] = {
    var source: BufferedSource = null
    var ret: Option[Array[String]] = None

    try {
      source = Source.fromFile(fileName)
      val lines = source.getLines()
      /* 去除首尾空格后，如果长度大于0，证明有内容 */
      val fileLines = lines.map(line => line.trim).filter(line => line.length > 0)
      ret = Some(fileLines.toArray)
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        ret = None
    } finally {
      if (source != null) source.close()
    }

    ret
  }

  /**
   * 读取文件的内容， 以键值对的形式返回
   * 异常时返回None
   *
   * @param fileName 文件名字
   * @param d, 切割键值对的分割符号
   * @param annotate, 注释行的标识，如果以annotate开头，跳过此行, 默认是'#'
   * @return
   */
  def readFile2kv(fileName: String, d: String, annotate: Char = '#'): Option[MuMap[String, String]] = {
    var source: BufferedSource = null
    var ret: Option[MuMap[String, String]] = None

    try {
      source = Source.fromFile(fileName)
      val lines = source.getLines()
      val kvs = MuMap[String, String]()

      for (line <- lines) {
        val lineContent = line.trim
        if (lineContent.length > 0 && lineContent.charAt(0) != annotate) { //空行和注释行跳过
        val kv = lineContent.split(d, 2)
          kvs += (kv(0).trim -> kv(1).trim)
        }
      }

      ret = Some(kvs)
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        ret = None
    } finally {
      if (source != null) source.close()
    }

    ret
  }

  /**
   * 如果你要操作的文件比较小，可以利用这个函数一次性获取文件的所有内容
   * 多用于调试
   * 发生异常的时候返回None
   * @param fileName 文件名字
   * @return
   */
  def mkStr(fileName: String): Option[String] = {
    var source: BufferedSource = null
    var str: Option[String] = None

    try {
      source = Source.fromFile(fileName)
      str = Some(source.mkString)
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        str = None
    } finally {
      if (source != null) source.close()
    }

    str
  }

  /**
   * 将内容存储到文件中，如果文件存在，将被覆盖
   * 如果文件不存在，会创建文件
   * 如果父目录不存在，会发生异常
   * @param fileName 文件名字
   * @param content 要写入的文件内容
   * @return
   */
  def save(fileName: String, content: String): Boolean = {
    var status = false
    var printer: PrintWriter = null

    try {
      printer = new PrintWriter(new File(fileName))
      printer.print(content)
      status = true
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        status = false
    } finally {
      if (printer != null) printer.close()
    }

    status
  }

  /**
   * 将数据存储到文件
   * 当apend为true时，追加内容到文件中
   * 如果需要换行，需要在每个content元素的尾部加入'\n'
   * @param fileName 文件名字
   * @param contents 内容
   * @param append 是否追加
   * @return
   */
  def save(fileName: String, contents: TraversableOnce[String], append: Boolean = false): Boolean = {
    var status = false
    var fos: FileOutputStream = null
    var bos: BufferedOutputStream = null

    try {
      fos = new FileOutputStream(fileName, append)
      bos = new BufferedOutputStream(fos)

      contents foreach { content =>
        bos.write(content.getBytes())
      }

      bos.flush()
      status = true
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        status = false
    } finally {
      if (bos != null) bos.close()
      if (fos != null) fos.close()
    }

    status
  }

  /**
   * 追加文件
   * @param fileName 文件名字
   * @param content 追加内容
   * @return
   */
  def append(fileName: String, content: String): Boolean = {
    var out: FileWriter = null
    var status = false

    try {
      out = new FileWriter(fileName, true)
      out.write(content)
      status = true
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        status = false
    } finally {
      if (out != null) out.close()
    }

    status
  }

  /**
   * 将键值对的内容储存到文件中
   * 如果文件不存在就创建文件
   * @param fileName 文件名字
   * @param contents 文件键值对内容
   * @param d  键值对分隔符
   * @param append 是否追加文件
   * @return
   */
  def save2kv(fileName: String, contents: MuMap[String, String], d: String, append: Boolean = false): Boolean = {
    var status = false
    var fos: FileOutputStream = null
    var bos: BufferedOutputStream = null

    try {
      fos = new FileOutputStream(fileName, append)
      bos = new BufferedOutputStream(fos)

      contents.keys foreach { key =>
        bos.write(s"$key$d${contents(key)}\n".getBytes())
      }

      status = true
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        status = false
    } finally {
      if (bos != null) bos.close()
      if (fos != null) fos.close()
    }

    status
  }

  /**
   * 创建一个目录，如果这个目录的父目录不存在，会同时创建这个父目录
   * @param dir 目录名字
   * @return
   */
  def createDir(dir: String): Boolean = {
    try {
      val file = new File(dir)
      file.mkdirs()
      true
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        false
    }
  }

  /**
   * 删除规则文件
   * 如果文件不存在， 会返回true
   * @param fileName 文件名字
   * @return
   */
  def deleteRegFile(fileName: String): Boolean = {
    try {
      val file = new File(fileName)
      if (file.exists() && file.isFile) { //只删除规则文件
        file.delete()
        true
      } else {
        if (!file.exists()) {
          true
        } else {
          //BDCError.logError(s"file $fileName not a regulation file")
          false
        }
      }
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        false
    }
  }

  /**
   * 利用递归操作，删除一个目录和其子目录
   * @param dir 目录
   * @return
   */
  private def toDeleteDir(dir: File): Boolean = {
    try {
      if (dir.isDirectory) {
        val childrenDir = dir.list()
        childrenDir foreach { subDir =>
          if (!toDeleteDir(new File(dir, subDir))) return false
        }
      }
      dir.delete()
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        false
    }
  }

  /**
   * 删除一个目录和其子目录
   * 如果目录不存在，返回true
   * @param dir 要删除的目录名字
   * @return
   */
  def deleteDir(dir: String): Boolean = {
    val file = new File(dir)
    if (!file.exists()) {
      true
    } else {
      toDeleteDir(file)
    }
  }

  /**
   * 获取目录里的规则文件名字，如果目录下存在子目录，不获取子目录下的文件名字
   * 发生异常返回None
   * @param dir 目录
   * @return
   */
  def getRegFilesNameAtDir(dir: String): Option[Array[String]] = {
    try {
      val fileNames = ArrayBuffer[String]()
      val path = new File(dir)
      val files = path.listFiles()

      files foreach { file =>
        if (file.isFile) fileNames += file.getName.trim
      }

      Some(fileNames.toArray)
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        None
    }
  }

  /**
   * 获取路径下的所有规则文件,包括子目录
   * @param dir 目录
   * @return
   */
  private def subDirs(dir: File): Iterator[File] = {
    val d = dir.listFiles.filter(_.isDirectory)
    val f = dir.listFiles.toIterator
    f ++ d.toIterator.flatMap(file => subDirs(file))
  }

  /**
   * 获取路径下的所有规则文件，包括子目录
   * 如果发生异常，返回None
   * @param dir 目录
   * @return
   */
  def getRegFilesNameAtDir_r(dir: String): Option[Array[String]] = {
    try {
      val fileNames = ArrayBuffer[String]()
      val path = new File(dir)
      val fileIterator = this.subDirs(path)

      fileIterator.foreach { i =>
        fileNames += i.getName
      }

      Some(fileNames.toArray)
    } catch {
      case ex: Exception =>
        //BDCError.logError(ex.getStackTraceString)
        None
    }
  }

}
