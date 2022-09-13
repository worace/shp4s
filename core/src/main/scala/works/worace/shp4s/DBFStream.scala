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

class DBFIterator(is: InputStream) extends Iterator[Map[String, DBFValue]] {
  private val reader: DBFReader = new DBFReader(is)
  private val fieldCount = reader.getFieldCount
  private val fields: Vector[DBFField] = (0 until fieldCount).map(reader.getField(_)).toVector
  private val rowCount = reader.getRecordCount()
  private var fetchedCount = 0

  private def readDBFField(row: DBFRow, field: DBFField): DBFValue = {
    field.getType match {
      case DBFDataType.CHARACTER      => DBFString(row.getString(field.getName))
      case DBFDataType.LONG           => DBFLong(row.getLong(field.getName))
      case DBFDataType.DATE           => DBFDate(row.getDate(field.getName))
      case DBFDataType.FLOATING_POINT => DBFFloat(row.getBigDecimal(field.getName))
      case DBFDataType.NUMERIC        => DBFNumeric(row.getBigDecimal(field.getName))
      case DBFDataType.LOGICAL        => DBFLogical(row.getBoolean(field.getName))
      case DBFDataType.VARCHAR        => DBFString(row.getString(field.getName))
      case _                          => DBFString("")
    }
  }

  def stream: fs2.Stream[IO, Map[String, DBFValue]] = {
    fs2.Stream.fromIterator[IO](this, 16)
  }

  def next(): Map[String, DBFValue] = {
    val row = reader.nextRow()
    val r = fields.map(field => field.getName -> readDBFField(row, field)).toMap
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
