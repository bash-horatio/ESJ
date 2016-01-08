package common.HBaseHelper

/**
 * Created by cwx on 15-11-2.
 */


import com.stumbleupon.async.Callback
import org.hbase.async._

import scala.collection.parallel.immutable.ParMap
import scala.language.implicitConversions
import scala.language.postfixOps


/**
 * Created by cwx on 15-11-2.
 */

case class Row(key: String, family: String, qualifersAndValues: Map[String,String])

object HBaseHelper {
  implicit def conv[A, B](f: B => A): Callback[A, B] = {
    new Callback[A, B]() {
      def call(b: B) = f(b)
    }
  }

  val config = new Config("./DynConfig/asynchbase.conf")
  val client = new HBaseClient(config)

  def setColumnValue(table: String, key: String, family: String, qualifer: String, value: String): Boolean = {
    if (ensureTableFamily(table, family) == false) {
      println("table or family no exist")
      return false
    }

    try {
      val putRequest = new PutRequest(table, key, family, qualifer, value)
      val put = client.put(putRequest).join

      true
    } catch {
      case ex: Exception => println(s"err: ${ex.getMessage}")
        false
    }
  }

  def setRow(table: String, row: Row): Boolean = {
    if (ensureTable(table) == false) {
      println("table no exist")
      return false
    }

    try {
      val key = row.key
      val family = row.family
      val qualifersAndValues = row.qualifersAndValues

      qualifersAndValues.foreach(e => {
        setColumnValue(table, key, family, e._1, e._2)
      })

      true
    } catch {
      case ex: Exception => println(s"err: ${ex.getMessage}")
        false
    }
  }

  def setRows(table: String, rows: Map[String, Row]): Boolean = {
    if (ensureTable(table) == false) {
      println("table no exist")
      return false
    }

    try {
      rows.keys foreach(uid =>{
        setRow(table, rows(uid))
      })

      true
    } catch {
      case ex: Exception => println(s"err: ${ex.getMessage}")
        false
    }
  }

  def getRows(table: String, keys: Iterable[String]): ParMap[String, Row] = {
    import scala.collection.mutable.{Map => muMap}
    if (ensureTable(table) == false) {
      println("table no exist")
      val map = ParMap[String, Row]()
      return map
    }

    try {
      val rows = muMap[String, Row]()
      val scanner = client.newScanner(table)
      scanner.setFilter(getFilterList(keys))
      val mkvs = scanner.nextRows().join
      val family = new String(mkvs.get(0).get(0).family)

      if (mkvs != null) {
        for (i <- 0 to (mkvs.size - 1)) {
          val kvs = mkvs.get(i)
          val key = new String(kvs.get(0).key)
          val qualifersAndValues = muMap[String, String]()
          for (j <- 0 to (kvs.size - 1)) {
            qualifersAndValues += (new String(kvs.get(j).qualifier) -> new String(kvs.get(j).value))
          }
          val qav = qualifersAndValues.toMap
          val row = new Row(key, family, qav)
          rows += (key -> row)
        }
      }

      rows.toMap.par
    } catch {
      case ex: Exception => println(s"err: ${ex.getMessage}")
        val map = Map[String, Row]()
        map.par
    }
  }

  def getRow(table: String, keys: Iterable[String]): Map[String, Row] = {
    import scala.collection.mutable.{Map => muMap}
    if (ensureTable(table) == false) {
      println("table no exist")
      null
    }

    try {
      val rows = muMap[String, Row]()
      val scanner = client.newScanner(table)
      scanner.setFilter(getFilterList(keys))
      val mkvs = scanner.nextRows().join
      val family = new String(mkvs.get(0).get(0).family)

      if (mkvs != null) {
        for (i <- 0 to (mkvs.size - 1)) {
          val kvs = mkvs.get(i)
          val key = new String(kvs.get(0).key)
          val qualifersAndValues = muMap[String, String]()
          for (j <- 0 to (kvs.size - 1)) {
            qualifersAndValues += (new String(kvs.get(j).qualifier) -> new String(kvs.get(j).value))
          }
          val qav = qualifersAndValues.toMap
          val row = new Row(key, family, qav)
          rows += (key -> row)
        }
      }

      rows.toMap
    } catch {
      case ex: Exception => println(s"err: ${ex.getMessage}")
        null
    }
  }

  def getAllVersionsOfRows(table: String, keys: Iterable[String]): ParMap[String, Row] = {
    import scala.collection.mutable.{Map => muMap}
    if (ensureTable(table) == false) {
      println("table no exist")
      val map = ParMap[String, Row]()
      return map
    }

    try {
      val rows = muMap[String, Row]()
      val scanner = client.newScanner(table)
      scanner.setMaxVersions(Integer.MAX_VALUE)
      scanner.setFilter(getFilterList(keys))
      val mkvs = scanner.nextRows().join
      val family = new String(mkvs.get(0).get(0).family)

      if (mkvs != null) {
        for (i <- 0 to (mkvs.size - 1)) {
          val kvs = mkvs.get(i)
          val key = new String(kvs.get(0).key)
          val qualifersAndValues = muMap[String, String]()
          for (j <- 0 to (kvs.size - 1)) {
            qualifersAndValues += (new String(kvs.get(j).qualifier) -> new String(kvs.get(j).value))
          }
          val qav = qualifersAndValues.toMap
          val row = new Row(key, family, qav)
          rows += (key -> row)
        }
      }

      rows.toMap.par
    } catch {
      case ex: Exception => println(s"err: ${ex.getMessage}")
        val map = Map[String, Row]()
        map.par
    }
  }

  private def getFilterList(keys: Iterable[String]): FilterList = {
    import java.util.ArrayList
    try {
      val arrayList = new ArrayList[ScanFilter]()
      keys.foreach(e => {
        val scanFilter = new RowFilter(CompareFilter.CompareOp.EQUAL, new SubstringComparator(e))
        arrayList.add(scanFilter)
      })
      val filter = new FilterList(arrayList, FilterList.Operator.MUST_PASS_ONE)

      filter
    } catch {
      case ex: Exception => null
    }
  }

  def ensureTable(tableNameStr: String): Boolean = {
    client.ensureTableExists(tableNameStr) addCallback {
      o: Object => true
    } addErrback {
      e: Exception => false
    } join
  }

  def ensureTableFamily(tableNameStr: String, family: String): Boolean = {
    client.ensureTableFamilyExists(tableNameStr, family) addCallback {
      o: Object => true
    } addErrback {
      e: Exception => false
    } join
  }

}
