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

case class ShapefileHeader()

object Core {
  val version: Codec[Unit] = constant(hex"e8030000")
  val emptyInt: Codec[Unit] = constant(hex"0000000")
  val fileCode: Codec[Unit] = constant(hex"000270a")
  val fileLength: Codec[Int] = int32
  val shapeType: Codec[Int] = int32L
  val xMin: Codec[Double] = doubleL
  val xMax: Codec[Double] = doubleL
  val yMin: Codec[Double] = doubleL
  val yMax: Codec[Double] = doubleL
  val zMin: Codec[Double] = doubleL
  val zMax: Codec[Double] = doubleL
  val mMin: Codec[Double] = doubleL
  val mMax: Codec[Double] = doubleL

  val header = fileCode :: emptyInt :: emptyInt ::
    emptyInt :: emptyInt :: emptyInt ::
    fileLength :: version :: shapeType ::
    xMin :: yMin :: xMax :: yMax :: zMin :: zMax ::
    mMin :: mMax

  case class BBox(xMin: Double, yMin: Double, xMax: Double, yMax: Double)
  case class RangedValues(min: Double, max: Double, values: Vector[Double])
  object RangedValues {
    val zero: RangedValues = RangedValues(0.0, 0.0, Vector())
  }
  sealed trait Shape
  case object NullShape extends Shape
  case class Point(x: Double, y: Double) extends Shape
  case class PointZ(x: Double, y: Double, z: Double) extends Shape
  case class MultiPoint(bbox: BBox, numPoints: Int, points: Vector[Point]) extends Shape
  case class MultiPointZ(
    bbox: BBox,
    points: Vector[Point],
    z: RangedValues,
    m: Option[RangedValues]
    // m: Option
    // zValues: Vector[Double]
    // mRange: MRange,
    // mValues: Vector[Double]
  ) extends Shape

  case class RecordHeader(recordNumber: Int, wordsLength: Int) {
    def byteLength: Int = wordsLength * 2
  }
  val recordHeader = (int32 :: int32).as[RecordHeader]
  val bbox = (doubleL :: doubleL :: doubleL :: doubleL).as[BBox]
  def rangedValues(num: Int) = (doubleL :: doubleL :: vectorOfN(provide(num), doubleL)).as[RangedValues]
  val point = (doubleL :: doubleL).as[Point]
  val nullShape = provide(NullShape)
  val multiPoint = (bbox :: int32L :: vector(point)).as[MultiPoint]
  val multiPointZBody = int32L.flatZip { numPoints =>
    vectorOfN(provide(numPoints), point) ::
    rangedValues(numPoints) ::
    // TODO: Not sure using RangedValues.zero is right here -- should be None
    // but it may never get called anyway because it's a unit codec?
    optional(lookahead(rangedValues(numPoints).unit(RangedValues.zero)), rangedValues(numPoints))
  }.xmap(
    { case ((_numPoints, contents)) => contents },
    { v: (Vector[Point] :: RangedValues :: Option[RangedValues] :: HNil) =>
      v match {
        case points :: _ => {
          (points.size, v)
        //   (points.size, points :: zRange :: HNil)
        //   // mrange
        }
      }
    }
  )
  val multiPointZ = (bbox :: multiPointZBody).as[MultiPointZ]
  // val multiPointZ =
  //   (bbox :: vectorOfN(int32L, point) :: zRange :: vector(double) :: mRange :: vector(double))
  //     .as[MultiPointZ]

  object ShapeType {
    val nullShape = 0
    val point = 1
    val multiPoint = 8
    val multiPointZ = 18
  }

  case class ShapeRecord(header: RecordHeader, shape: Shape)
  val shape = recordHeader
    .flatPrepend { header =>
      fixedSizeBytes(
        header.byteLength,
        discriminated[Shape]
          .by(int32L)
          .subcaseO(ShapeType.nullShape) {
            case n: NullShape.type => Some(NullShape)
            case _                 => None
          }(nullShape)
          .subcaseO(ShapeType.point) {
            case p: Point => Some(p)
            case _        => None
          }(point)
          .subcaseO(ShapeType.multiPoint) {
            case mp: MultiPoint => Some(mp)
            case _              => None
          }(multiPoint)
          .subcaseO(ShapeType.multiPointZ) {
            case mpz: MultiPointZ => Some(mpz)
            case _                => None
          }(multiPointZ)
      ).hlist
      // subcaseO(2) { case (nme, fld: StringField) => Some(nme -> fld); case _ => None } (cstring ~ cstring.as[StringField])
    }
    .as[ShapeRecord]

  val shpStream: StreamDecoder[ShapeRecord] = StreamDecoder
    .many(shape)

  // val streamier: StreamDecoder[ShapeRecord] = for {
  //   header <- StreamDecoder.once(header)
  //   shape <- StreamDecoder.many(shape)
  // } yield {
  //   println("RUNNING STREAMIER")
  //   println(header)
  //   println("shape stream")
  //   println(shape)
  //   shape
  // }

  // val frames: StreamDecoder[ByteVector] = StreamDecoder
  //   .many(int32)
  //   .flatMap { numBytes => StreamDecoder.once(bytes(numBytes)) }

  // val filePath = java.nio.file.Paths.get("path/to/file")

  // implicit val csIO: ContextShift[IO] =
  //   IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  // val s: fs2.Stream[IO, ByteVector] =
  //   fs2.Stream.resource(Blocker[IO]).flatMap { blocker =>
  //     fs2.io.file.readAll[IO](filePath, blocker, 4096).through(frames.toPipeByte)
  //   }

  val HEADER_SIZE = 100
  def streamShapefile(path: Path)(implicit cs: ContextShift[IO]): fs2.Stream[IO, ShapeRecord] = {
    fs2.Stream.resource(Blocker[IO]).flatMap { blocker =>
      fs2.io.file
        .readAll[IO](path, blocker, 4096)
        .drop(HEADER_SIZE)
        .through(shpStream.toPipeByte)
    }
  }

  def readAllSync(path: Path)(implicit cs: ContextShift[IO]): Vector[ShapeRecord] = {
    streamShapefile(path).compile.toVector.unsafeRunSync()
  }

  // val shape: Codec[Shape] = recordHeader.flatPrepend { case header =>
  // TODO - header length is 16-bit words
  //   fixedSizeBytes(header.length, shapeDiscrim)
  // }

  // Header
  // Records:
  //   Record Header: Shape Type (int), Length (int)
  //   Record: Shape Type (int), Length#Bytes
  // Flat:
  // 10 x 16 = 160 bits; (4 + 8 + 8) x 8 = 160
  // 4               4                               4               8           8
  // 1 (Point Type), 10 (Point size - 16 bit words), 1 (Point Type), X (Double), Y (Double)
  def readHeader(bytes: Array[Byte]): Option[ShapefileHeader] = {
    println(s"total size: ${bytes.size}")
    val r = for {
      h <- header.decode(BitVector(bytes))
      shp1 <- shape.decode(h.remainder)
      // shp2 <- shape.decode(shp1.remainder)
      // shp3 <- shape.decode(shp2.remainder)
    } yield {
      println("header")
      println(h)
      println(s"used: ${(bytes.size - h.remainder.size / 8)}")
      println(s"leftover: ${h.remainder.size / 8}")
      println("decoded shp1 ********")
      println(shp1)
      // println(s"leftover: ${shp1.remainder.size / 8}")
      // // point size should be 4+ 4 + 4 +8 + 8 = 28
      // println(s"used: ${(h.remainder.size - shp1.remainder.size) / 8}")
      // println("decoded shp2 ********")
      // println(shp2)
      // println(s"leftover: ${shp2.remainder.size / 8}")
      // println("SHP 3")
      // println(shp3)
    }
    println("result:")
    println(r)
    None
  }
}
