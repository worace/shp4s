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

case class ShapefileHeader()

object Core {
  val headerint: Codec[Int] = int32
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

  sealed trait Shape
  case class Point(x: Double, y: Double) extends Shape

  case class RecordHeader(shapeType: Int, length: Int)
  val recordHeader = (int32 :: int32).as[RecordHeader]
  val point = (doubleL :: doubleL).as[Point]

  val shapeDiscrim = discriminated[Shape]
    .by(int32L)
    .typecase(1, point)

  case class ShapeRecord(header: RecordHeader, shape: Shape)
  val shape = recordHeader.flatPrepend { header =>
    discriminated[Shape]
      .by(uint8)
      .subcaseO(1) {
        case p: Point => Some(p)
        case _        => None
      }(point)
      .hlist
  // subcaseO(2) { case (nme, fld: StringField) => Some(nme -> fld); case _ => None } (cstring ~ cstring.as[StringField])
  }.as[ShapeRecord]

  val shpStream: StreamDecoder[ShapeRecord] = StreamDecoder
    .many(shape)

  val frames: StreamDecoder[ByteVector] = StreamDecoder
    .many(int32)
    .flatMap { numBytes => StreamDecoder.once(bytes(numBytes)) }

  val filePath = java.nio.file.Paths.get("path/to/file")

  // implicit val csIO: ContextShift[IO] =
  //   IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  // val s: fs2.Stream[IO, ByteVector] =
  //   fs2.Stream.resource(Blocker[IO]).flatMap { blocker =>
  //     fs2.io.file.readAll[IO](filePath, blocker, 4096).through(frames.toPipeByte)
  //   }

  def streamShapefile(path: Path)(implicit cs: ContextShift[IO]): fs2.Stream[IO, ShapeRecord] = {
    ???
    fs2.Stream.resource(Blocker[IO]).flatMap { blocker =>
      fs2.io.file.readAll[IO](path, blocker, 4096)
        .through(shpStream.toPipeByte)
    }
  }

  // val shape: Codec[Shape] = recordHeader.flatPrepend { case header =>
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
    println("header codec")
    println(header)
    println(BitVector(bytes))
    println(headerint.decode(BitVector(bytes)))

    for {
      h <- header.decode(BitVector(bytes))
      shp <- shape.decode(h.remainder)
    } yield {
      println("header")
      println(h)
      println("decoded shp ********")
      println(shp)
    }

    for {
      h <- header.decode(BitVector(bytes))
      rec <- recordHeader.decode(h.remainder)
      p <- point.decode(rec.remainder)
      rec2 <- recordHeader.decode(p.remainder)
      p2 <- point.decode(rec2.remainder)
    } yield {
      println("header:")
      println(h)
      println("first rec:")
      println(rec)
      println("point contents:")
      println(p)

      println("rec2")
      println(rec2)
      println("point2")
      println(p2)
    }
    None
  }

  def hi: String = "hi"
}
