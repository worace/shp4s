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

case class ShapefileHeader(
  length: Int, shapeType: Int, xMin: Double, xMax: Double, yMin: Double, yMax: Double,
  zMin: Double, zMax: Double, mMin: Double, mMax: Double
)

object Core {
  val fileCode: Codec[Unit] = constant(hex"000270a")
  val emptyInt: Codec[Unit] = constant(hex"0000000")
  val version: Codec[Unit] = constant(hex"e8030000")
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

  val header = (fileCode :: emptyInt :: emptyInt ::
    emptyInt :: emptyInt :: emptyInt ::
    fileLength :: version :: shapeType ::
    xMin :: yMin :: xMax :: yMax :: zMin :: zMax ::
    mMin :: mMax).xmap(
    { case (_fc :: _ :: _ :: _ :: _ :: _ :: length :: _version ::
      shapeType :: xMin :: yMin :: xMax :: yMax :: zMin :: zMax :: mMin :: mMax :: HNil) =>
      ShapefileHeader(length, shapeType, xMin, xMax, yMin, yMax, zMin, zMax, mMin, mMax)
    },
      (h: ShapefileHeader) => () :: () :: () :: () :: () :: () :: h.length :: () ::
        h.shapeType ::
        h.xMin :: h.yMin :: h.xMax :: h.yMax :: h.zMin :: h.zMax :: h.mMin :: h.mMax :: HNil
  )

  case class BBox(xMin: Double, yMin: Double, xMax: Double, yMax: Double)
  case class RangedValues(min: Double, max: Double, values: Vector[Double])
  object RangedValues {
    val zero: RangedValues = RangedValues(0.0, 0.0, Vector())
  }
  sealed trait Shape
  case object NullShape extends Shape
  case class Point(x: Double, y: Double) extends Shape
  case class PointZ(x: Double, y: Double, z: Double, m: Option[Double]) extends Shape
  case class MultiPoint(bbox: BBox, points: Vector[Point]) extends Shape
  case class MultiPointZ(
    bbox: BBox,
    points: Vector[Point],
    z: RangedValues,
    m: Option[RangedValues]
  ) extends Shape
  case class PolyLine(bbox: BBox, lines: Vector[Vector[Point]]) extends Shape
  case class Polygon(bbox: BBox, rings: Vector[Vector[Point]]) extends Shape

  val MIN_SHP_DOUBLE: Double = -1.0E38
  object PointZ {
    def apply(x: Double, y: Double, z: Double, m: Double): PointZ = {
      PointZ(x, y, z, Some(m).filter(_ > MIN_SHP_DOUBLE))
    }
  }

  case class RecordHeader(recordNumber: Int, wordsLength: Int) {
    def byteLength: Int = wordsLength * 2
    def bitLength: Int = byteLength * 8
  }
  val recordHeader = (int32 :: int32).as[RecordHeader]
  val bbox = (doubleL :: doubleL :: doubleL :: doubleL).as[BBox]
  def rangedValues(num: Int) = (doubleL :: doubleL :: vectorOfN(provide(num), doubleL)).as[RangedValues]

  def ifAvailable[A](codec: Codec[A], zero: A): Codec[Option[A]] = {
    optional(lookahead(codec.unit(zero)), codec)
  }

  val multiPointZBody = int32L.flatZip { numPoints =>
    vectorOfN(provide(numPoints), ShpCodecs.point) ::
    rangedValues(numPoints) ::
    // TODO: Not sure using RangedValues.zero is right here -- should be None
    // but it may never get called anyway because it's a unit codec?
    ifAvailable(rangedValues(numPoints), RangedValues.zero)
  }.xmap(
    { case ((_numPoints, contents)) => contents },
    { v: (Vector[Point] :: RangedValues :: Option[RangedValues] :: HNil) =>
      v match {
        case points :: _ => {
          (points.size, v)
        }
      }
    }
  )



  private case class PolyLineHeader(bbox: BBox, numParts: Int, numPoints: Int)
  private val polylineHeader = (bbox :: int32L :: int32L).as[PolyLineHeader]

  private def polylineSlices(points: Vector[Point], offsets: Vector[Int]): Vector[Vector[Point]] = {
    if (offsets.size > 1) {
      offsets
        .sliding(2)
        .map { case Vector(start, finish) => points.slice(start, finish) }
        .toVector
    } else {
      Vector(points)
    }
  }


  case class ShapeRecord(header: RecordHeader, shape: Shape)

  object ShpCodecs {
    val point = (doubleL :: doubleL).as[Point]
    val nullShape = provide(NullShape)
    val multiPoint = (bbox :: int32L.consume(
      numPoints => vectorOfN(provide(numPoints), point)
    )(points => points.size)).as[MultiPoint]
    val multiPointZ = (bbox :: multiPointZBody).as[MultiPointZ]
    val polyline: Codec[PolyLine] = polylineHeader.flatPrepend { h =>
      vectorOfN(provide(h.numParts), int32L).hlist
    }.flatPrepend { case h :: _ =>
        vectorOfN(provide(h.numPoints), point).hlist
    }.xmap(
      { case ((header :: offsets :: HNil) :: points :: HNil) =>
        PolyLine(header.bbox, polylineSlices(points, offsets)) },
      (pl: PolyLine) => {
        val points = pl.lines.flatten
        val numPoints = points.size
        val offsets = pl.lines.map(_.size - 1).prepended(0)
        val header = PolyLineHeader(pl.bbox, pl.lines.size, numPoints)
        (header :: offsets :: HNil) :: points :: HNil
      }
    )
    val polygon: Codec[Polygon] = polyline.xmap(
      pl => Polygon(pl.bbox, pl.lines),
      poly => PolyLine(poly.bbox, poly.rings)
    )

    val pointZ = (doubleL :: doubleL :: doubleL :: ifAvailable(doubleL, Double.MinValue)).xmap(
      { case (x :: y :: z :: m :: HNil) => PointZ(x, y, z, m)  },
      (pz: PointZ) => pz.x :: pz.y :: pz.z :: pz.m :: HNil
    )

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
            .subcaseO(ShapeType.polyline) {
              case pl: PolyLine => Some(pl)
              case _                => None
            }(polyline)
            .subcaseO(ShapeType.polygon) {
              case p: Polygon => Some(p)
              case _                => None
            }(polygon)
            .subcaseO(ShapeType.pointZ) {
              case p: PointZ => Some(p)
              case o                => None
            }(pointZ)
        ).hlist
      }
      .as[ShapeRecord]
  }

  object ShapeType {
    val nullShape = 0
    val point = 1
    val polyline = 3
    val polygon = 5
    val multiPoint = 8
    val pointZ = 11
    val multiPointZ = 18
  }

  val shpStream: StreamDecoder[ShapeRecord] = StreamDecoder
    .many(ShpCodecs.shape)

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

  // Header
  // Records:
  //   Record Header: Shape Type (int), Length (int)
  //   Record: Shape Type (int), Length#Bytes
  // Flat:
  // 10 x 16 = 160 bits; (4 + 8 + 8) x 8 = 160
  // 4               4                               4               8           8
  // 1 (Point Type), 10 (Point size - 16 bit words), 1 (Point Type), X (Double), Y (Double)
  def readHeader(bytes: Array[Byte]): Try[ShapefileHeader] = {
    header.decode(BitVector(bytes)).toTry.map(_.value)
  }
}
// Polygon -- same as polyline
// bbox
// numparts (int) == num rings
// numpoints (int)


// Types - 14
// * [X] NullShape
// * [X] Point
// * [X] PolyLine
// * [X] Polygon
// * [X] MultiPoint
// * [ ] PointZ
// * [ ] PolylineZ
// * [ ] PolygonZ
// * [X] MultiPointZ
// * [ ] PointM
// * [ ] PolyLineM
// * [ ] PolygonM
// * [ ] MultiPointM
// * [ ] MultiPatch

// TODO Edge Cases
// * [ ] Verify PointZ file with M values
