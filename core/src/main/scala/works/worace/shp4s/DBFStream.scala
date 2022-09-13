package works.worace.shp4s

import java.io.FileInputStream
import com.linuxense.javadbf.{DBFReader, DBFField}
import java.nio.file.Path
import java.util.Date
import scala.math.BigDecimal
import com.linuxense.javadbf.DBFRow
import com.linuxense.javadbf.DBFDataType
import cats.effect.IO
import java.io.InputStream

sealed trait DBFValue
case class DBFString(value: String) extends DBFValue
case class DBFLong(value: Long) extends DBFValue
case class DBFDate(value: Date) extends DBFValue
case class DBFFloat(value: BigDecimal) extends DBFValue
case class DBFNumeric(value: BigDecimal) extends DBFValue
case class DBFLogical(value: Boolean) extends DBFValue
case object DBFNull extends DBFValue

class DBFIterator(is: InputStream) extends Iterator[Map[String, DBFValue]] {
  private val reader: DBFReader = new DBFReader(is)
  private val fieldCount = reader.getFieldCount
  private val fields: Vector[DBFField] = (0 until fieldCount).map(reader.getField(_)).toVector
  private val rowCount = reader.getRecordCount()
  private var fetchedCount = 0

  private implicit class DbfRowExt(row: DBFRow) {
    def strOpt(field: DBFField): Option[String] = {
      Option(row.getString(field.getName))
    }
    def longOpt(field: DBFField): Option[Long] = {
      Option(row.getLong(field.getName))
    }
    def dateOpt(field: DBFField): Option[Date] = {
      Option(row.getDate(field.getName))
    }
    def bigDecimalOpt(field: DBFField): Option[java.math.BigDecimal] = {
      Option(row.getBigDecimal(field.getName))
    }
    def boolOpt(field: DBFField): Option[Boolean] = {
      Option(row.getBoolean(field.getName))
    }
  }

  private def readDBFField(row: DBFRow, field: DBFField): Option[DBFValue] = {
    field.getType match {
      case DBFDataType.CHARACTER      => row.strOpt(field).map(DBFString)
      case DBFDataType.LONG           => row.longOpt(field).map(DBFLong)
      case DBFDataType.DATE           => row.dateOpt(field).map(DBFDate)
      case DBFDataType.FLOATING_POINT => row.bigDecimalOpt(field).map(DBFFloat(_))
      case DBFDataType.NUMERIC        => row.bigDecimalOpt(field).map(DBFNumeric(_))
      case DBFDataType.LOGICAL        => row.boolOpt(field).map(DBFLogical)
      case DBFDataType.VARCHAR        => row.strOpt(field).map(DBFString)
      case _                          => None
    }
  }

  def stream: fs2.Stream[IO, Map[String, DBFValue]] = {
    fs2.Stream.fromIterator[IO](this, 16)
  }

  def next(): Map[String, DBFValue] = {
    val row = reader.nextRow()
    val r = fields.map(field => field.getName -> readDBFField(row, field).getOrElse(DBFNull)).toMap
    fetchedCount += 1
    r
  }

  def hasNext: Boolean = fetchedCount < rowCount
  def close(): Unit = reader.close()
}

object DBFIterator {
  def apply(path: Path): DBFIterator = new DBFIterator(new FileInputStream(path.toString))
  def apply(is: InputStream): DBFIterator = new DBFIterator(is)
}
