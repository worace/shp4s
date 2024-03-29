package works.worace.shp4s

import scodec.bits._
import scodec._
import scodec.codecs._
import cats.effect.IO
import scala.collection.immutable.{Stream => _, _}
import java.nio.file.Path
import scala.util.Try
import java.nio.file.Paths
import fs2.io.file.Files

trait ShpCodec[T] {
  def codec: Codec[T]
  def decode(bits: BitVector): Attempt[DecodeResult[T]] = codec.decode(bits)
}

case class BBox(xMin: Double, yMin: Double, xMax: Double, yMax: Double)
case class Range(min: Double, max: Double)

case class RangedValues(min: Double, max: Double, values: Vector[Double]) {
  def range: Range = Range(min, max)
}

object RangedValues {
  val zero: RangedValues = RangedValues(0.0, 0.0, Vector())
}

case class RecordHeader(recordNumber: Int, wordsLength: Int) {
  def byteLength: Int = wordsLength * 2
  def bitLength: Int = byteLength * 8
}

object RecordHeader extends ShpCodec[RecordHeader] {
  val codec = (int32 :: int32).as[RecordHeader]
}

object ShapeType {
  val nullShape = 0
  val point = 1
  val polyLine = 3
  val polygon = 5
  val multiPoint = 8
  val pointZ = 11
  val polyLineZ = 13
  val polygonZ = 15
  val multiPointZ = 18
  val pointM = 21
  val polyLineM = 23
  val polygonM = 25
  val multiPointM = 28
}

case class ShapeRecord(header: RecordHeader, shape: Shape)

object Core {
  type Props = Map[String, DBFValue]
  val HEADER_SIZE = 100
  private def featureStream(
    shapes: fs2.Stream[IO, ShapeRecord],
    props: fs2.Stream[IO, Map[String, DBFValue]]
  ): fs2.Stream[IO, Feature] = {
    shapes.zip(props).map {
      case (shape, props) =>
        Feature(shape.header.recordNumber, shape.shape, props)
    }
  }

  private def streamShapeRecords(shpStream: fs2.Stream[IO, Byte]): fs2.Stream[IO, ShapeRecord] = {
    shpStream
      .drop(HEADER_SIZE)
      .through(Codecs.shpStream.toPipeByte)
  }

  def streamShapefile(path: Path): fs2.Stream[IO, Feature] = {
    val dbfPath = path.resolveSibling(path.getFileName.toString.replace(".shp", ".dbf"))
    val props = DBFIterator(dbfPath).stream
    val shapes = streamShapeRecords(Files[IO].readAll(fs2.io.file.Path.fromNioPath(path)))

    featureStream(shapes, props)
  }

  def streamShapefile(path: String): fs2.Stream[IO, Feature] = {
    streamShapefile(Paths.get(path))
  }

  def readAllSyncFromPath(path: Path)(
    implicit runtime: cats.effect.unsafe.IORuntime = cats.effect.unsafe.implicits.global
  ): Vector[Feature] = {
    streamShapefile(path).compile.toVector.unsafeRunSync()
  }

  def readAllSync(path: String)(
    implicit runtime: cats.effect.unsafe.IORuntime = cats.effect.unsafe.implicits.global
  ): Vector[Feature] = {
    readAllSyncFromPath(Paths.get(path))
  }

  def readHeader(bytes: Array[Byte]): Try[FileHeader] = {
    FileHeader.codec.decode(BitVector(bytes)).toTry.map(_.value)
  }
}
