package works.worace.shp4s

import java.io.FileInputStream
import com.linuxense.javadbf.{DBFReader, DBFField}
import java.nio.file.Path
import scala.util.Try

class DBFIterator(path: Path) extends Iterator[Map[String, String]] {
  val reader: DBFReader = new DBFReader(new FileInputStream(path.toString))
  val fieldCount = reader.getFieldCount
  val fields: Vector[DBFField] = (0 until fieldCount).map(reader.getField(_)).toVector
  val fieldNames: Vector[String] = fields.map(_.getName)
  private val rowCount = reader.getRecordCount()
  private var fetchedCount = 0

  // for (int i = 0; i < numberOfFields; i++) {

  // 	DBFField field = reader.getField(i);

  // 	// do something with it if you want
  // 	// refer the JavaDoc API reference for more details
  // 	//
  // 	System.out.println(field.getName());
  // }

  def next(): Map[String, String] = {
    val row = reader.nextRow()
    val intex = row.getInt("LABEL_FLAG")
    println(s"int field:  ${intex}")
    val r = fieldNames.map(n => n -> Try(row.getString(n)).getOrElse("")).toMap
    fetchedCount += 1
    r
  }

  def hasNext: Boolean = fetchedCount < rowCount
}
