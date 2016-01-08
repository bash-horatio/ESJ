/*
 * Copyright (c) 2015, BoDao, Inc. All Rights Reserved.
 *
 * Author@ dgl
 *
 * This software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.
 *
 *
 * you can operate file Conveniently by this software.
 *
 * apend data to file
 * get file name at dir
 * clean file content
 * get process running dir
 * etc.
 *
 */


package common.FileHelper


import java.io.{File, _}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object FileHelper {

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


	/* clean file context, the file context will be void */
	def cleanFile(file: String): Boolean = {
		try {
			val f = new File(file)
			val fw =  new FileWriter(f)
			fw.write("")
			fw.close()
			true
		} catch  {
			case ex: Exception =>
				println("clean file fail")
				false
		}
	}

	/* get prog current working dir */
	def currentDir(): String = {
		val directory = new File("")
		try{
			directory.getAbsolutePath()
		} catch {
			case ex: Exception => {
				Unit.toString()
			}
		}
	}

	/* get all lines in file */
	def getFileLines(file: String): ArrayBuffer[String] = {
		val fileLines = new ArrayBuffer[String]()

		try {
			val source = Source.fromFile(file)
			val lines = source.getLines()

			for (line <- lines) fileLines += line

			source.close()
		} catch {
			case ex: Exception => println(ex.getMessage())
		}

		fileLines
	}

	/* all lines will be removed ' ' and '\n' */
	def getFileLinesTrim(file: String): ArrayBuffer[String] = {
		val fileLines = new ArrayBuffer[String]()
		try {
			val source = Source.fromFile(file)
			val lines = source.getLines()
			for (line <- lines) {
				val l = line.trim
				if (l != "") fileLines += l
			}
			source.close()
		} catch {
			case ex: Exception =>
				println(ex.getMessage())
		}

		fileLines
	}

	/* file is exist ? */
	def fileIsExist(file: String): Boolean = {
		val f = new File(file)
		if (f.exists()) true else false
	}

	/* get the file context to a string */
	def readFile(file: String): String = {
		if (!fileIsExist(file)) {
			println(file + " is not exist")
			Unit.toString()
		}
		val source = Source.fromFile(file)
		val data = source.mkString

		source.close()
		data
	}

	/* save a string to file, file context will be clean */
	def save2File(file: String, data: String): Boolean = {
		try {
			val svaeFile = new PrintWriter(new File(file))
			svaeFile.print(data)
			svaeFile.close()
		} catch {
			case ex: FileNotFoundException => {
				println(file + " is not found")
				false
			}
			case ex: IOException => {
				println("svae to file " + file + " error ")
				false
			}
		}

		true
	}

	/* save string to file, apend it */
	def wirteFileApend(file: String, data: String): Boolean = {
		try {
			val out = new java.io.FileWriter(file, true)
			out.write(data)
			out.close()
			true
		} catch {
			case ex: Exception => {
				println(ex.getMessage())
				false
			}
		}
	}

	/* create a dir, if it's parent dir is not Exist, create it's parent dir */
	def createDir(dir: String): Boolean = {
		try {
			val file = new File(dir)
			file.mkdirs()
			true
		} catch {
			case ex: Exception =>
				false
		}
	}

	/* create a file, you must know the file's dir and name */
	def createFile(dir: String, fileName: String): Boolean = {
		if (!createDir(dir))  false
		try {
			val fileAbus = dir.stripSuffix("/") + "/" + fileName
			val file = new File(fileAbus)
			file.createNewFile()
			true
		} catch {
			case ex: Exception =>
				false
		}
	}

	def deleteFile(file: String): Boolean = {
		try {
			if (fileIsExist(file)) {
				new File(file).delete()
			}
			true
		} catch {
			case ex: Exception =>
				false
		}

		false
	}

	/* get the file name at dir, if exist sub dir, not get the subdir's file name */
	def getRegFileNameAtDir(dir: String): ArrayBuffer[String] = {
		val fileNames = ArrayBuffer[String]()
		fileNames.sortWith(_ > _)
		val path = new File(dir)
		val files = path.listFiles()

		for (f <- files) if (f.isFile()) fileNames += f.getName().trim

		fileNames
	}

	def subDirs(dir: File): Iterator[File] = {
		val d = dir.listFiles.filter(_.isDirectory)
		val f = dir.listFiles.toIterator
		f ++ d.toIterator.flatMap(subDirs _)
	}

	/* get the file name at dir, if exist sub dir, will get the file name at sub dir */
	def getRegFileNameAtDir_R(dir: String): ArrayBuffer[String] = {
		val fileNames = ArrayBuffer[String]()
		val path = new File(dir)
		val it = subDirs(path)
		it.foreach(i => fileNames += i.getName())

		fileNames
	}
}
