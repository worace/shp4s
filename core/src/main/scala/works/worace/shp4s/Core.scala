package works.worace.shp4s

import scodec.bits._
import scodec._
import scodec.codecs._
import scodec.Attempt.Successful
import shapeless.HNil
import scodec.stream._
import cats.effect.IO
import scala.collection.immutable.{Stream => _, _}
import cats.effect.Blocker
import cats.effect.ContextShift
import java.nio.file.Path
import shapeless.::
import scala.util.Try
import java.nio.file.Paths

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
  val multiPointM = 28
}

case class ShapeRecord(header: RecordHeader, shape: Shape)

object Core {
  val HEADER_SIZE = 100
  def streamShapefile(path: Path)(implicit cs: ContextShift[IO]): fs2.Stream[IO, Feature] = {
    val dbfPath = path.resolveSibling(path.getFileName.toString.replace(".shp", ".dbf"))
    val dbfReader = new DBFIterator(dbfPath)

    val props = fs2.Stream.fromIterator[IO](dbfReader)
    val shapes = fs2.Stream.resource(Blocker[IO]).flatMap { blocker =>
      fs2.io.file
        .readAll[IO](path, blocker, 4096)
        .drop(HEADER_SIZE)
        .through(Codecs.shpStream.toPipeByte)
    }

    shapes.zip(props).map {
      case (shape, props) =>
        Feature(shape.header.recordNumber, shape.shape, props)
    }
  }

  def streamShapefile(path: String)(implicit cs: ContextShift[IO]): fs2.Stream[IO, Feature] = {
    streamShapefile(Paths.get(path))
  }

  def readAllSync(path: Path)(implicit cs: ContextShift[IO]): Vector[Feature] = {
    streamShapefile(path).compile.toVector.unsafeRunSync()
  }

  def readAllSync(path: String)(implicit cs: ContextShift[IO]): Vector[Feature] = {
    readAllSync(Paths.get(path))
  }

  def readHeader(bytes: Array[Byte]): Try[FileHeader] = {
    FileHeader.codec.decode(BitVector(bytes)).toTry.map(_.value)
  }
}
