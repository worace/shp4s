package works.worace.shp4s

import scodec._
import scodec.codecs._
import shapeless.HNil
import shapeless.::
import fs2.interop.scodec._

private object Codecs {
  val nullShape = provide(NullShape)
  val point = (doubleL :: doubleL).as[Point]
  val bbox = (doubleL :: doubleL :: doubleL :: doubleL).as[BBox]

  val pointZ = (doubleL :: doubleL :: doubleL :: Util.ifAvailable(doubleL, Double.MinValue)).xmap(
    { case (x :: y :: z :: m :: HNil) => PointZ(x, y, z, m) },
    (pz: PointZ) => pz.x :: pz.y :: pz.z :: pz.m :: HNil
  )

  val multiPoint =
    (bbox :: int32L.consume(numPoints => vectorOfN(provide(numPoints), point))(points =>
      points.size
    )).as[MultiPoint]

  val multiPointM: Codec[MultiPointM] = (Codecs.bbox :: int32L)
    .flatPrepend {
      case _ :: numPoints :: HNil =>
        vectorOfN(provide(numPoints), point) :: Util.rangedValues(numPoints)
    }
    .xmap(
      {
        case ((bbox :: _ :: HNil) :: points :: mVals :: HNil) => {
          val pointMs =
            points.zip(mVals.values).map { case (point, m) => PointM(point.x, point.y, m) }
          MultiPointM(bbox, mVals.range, pointMs)
        }
      },
      (mp: MultiPointM) => {
        (mp.bbox :: mp.points.size :: HNil) :: mp.points.map(_.pointXY) :: mp.mRangedValues :: HNil
      }
    )

  val multiPointZ: Codec[MultiPointZ] = (Codecs.bbox :: int32L)
    .flatPrepend {
      case _ :: numPoints :: HNil =>
        vectorOfN(provide(numPoints), point) :: Util.rangedValues(numPoints) :: Util.ifAvailable(
          Util.rangedValues(numPoints),
          RangedValues.zero
        )
    }
    .xmap(
      {
        case ((bbox :: _ :: HNil) :: points :: zVals :: mVals :: HNil) => {
          val pointZs = mVals match {
            case Some(mVals) => {
              points.zip(zVals.values).zip(mVals.values).map {
                case ((point, z), m) => PointZ(point.x, point.y, z, Some(m))
              }
            }
            case None => {
              points.zip(zVals.values).map { case (point, z) => PointZ(point.x, point.y, z, None) }
            }
          }
          MultiPointZ(bbox, pointZs, zVals.range, mVals.map(_.range))
        }
      },
      (mp: MultiPointZ) => {
        (mp.bbox :: mp.points.size :: HNil) :: mp.points
          .map(_.pointXY) :: mp.zRangedValues :: mp.mRangedValues :: HNil
      }
    )

  case class PolyLineHeader(bbox: BBox, numParts: Int, numPoints: Int)
  val polyLineHeader = (bbox :: int32L :: int32L).as[PolyLineHeader]

  val polyLine: Codec[PolyLine] = polyLineHeader
    .flatPrepend { h => vectorOfN(provide(h.numParts), int32L).hlist }
    .flatPrepend {
      case h :: _ =>
        vectorOfN(provide(h.numPoints), point).hlist
    }
    .xmap(
      {
        case ((header :: offsets :: HNil) :: points :: HNil) =>
          PolyLine(header.bbox, Util.offsetSlices(points, offsets))
      },
      (pl: PolyLine) => {
        val points = pl.lines.flatten
        val numPoints = points.size
        val offsets = Vector(0) ++ pl.lines.map(_.size - 1)
        val header = PolyLineHeader(pl.bbox, pl.lines.size, numPoints)
        (header :: offsets :: HNil) :: points :: HNil
      }
    )
  val polygon: Codec[Polygon] = polyLine.xmap(
    pl => Polygon(pl.bbox, pl.lines),
    poly => PolyLine(poly.bbox, poly.rings)
  )

  val pointM = (doubleL :: doubleL :: doubleL).xmap(
    { case (x :: y :: m :: HNil) => PointM(x, y, m) },
    (p: PointM) => p.x :: p.y :: p.m :: HNil
  )

  val polyLineM = polyLine
    .flatPrepend { pl => Util.rangedValues(pl.numPoints).hlist }
    .xmap(
      {
        case pl :: mVals :: HNil => {
          val pointMs = Util.zippedWithFlatVec(pl.lines, mVals.values) { (point, m) =>
            PointM(point.x, point.y, m)
          }
          PolyLineM(pl.bbox, mVals.range, pointMs)
        }
      },
      (pl: PolyLineM) => {
        val pointsXY = pl.lines.map(line => line.map(p => Point(p.x, p.y)))
        val mVals = Util.ringRangedValues(pl.mRange, pl.lines) { p => p.m }
        PolyLine(pl.bbox, pointsXY) :: mVals :: HNil
      }
    )

  val polygonM: Codec[PolygonM] = polyLineM.xmap(
    pl => PolygonM(pl.bbox, pl.mRange, pl.lines),
    poly => PolyLineM(poly.bbox, poly.mRange, poly.rings)
  )

  // Codec for polylineZ based on reading polyline and then handling the trailing z/m values
  val polyLineZ = polyLine
    .flatPrepend { pl =>
      Util.rangedValues(pl.numPoints) :: Util.ifAvailable(
        Util.rangedValues(pl.numPoints),
        RangedValues.zero
      )
    }
    .xmap(
      {
        case pl :: zRanged :: mRanged :: HNil => {
          val zs = zRanged.values
          val ms = mRanged.map(_.values).getOrElse(Vector())
          val pointZs = Util.zippedWithFlatVecTriple(pl.lines, zs, ms) { (point, z, m) =>
            PointZ(point.x, point.y, z.getOrElse(0.0), m)
          }
          PolyLineZ(pl.bbox, zRanged.range, mRanged.map(_.range), pointZs)
        }
      },
      (plz: PolyLineZ) => {
        val pointsXY = plz.lines.map(line => line.map(p => Point(p.x, p.y)))
        val zVals = Util.pointZRingsZValues(plz.zRange, plz.lines)
        val mVals = Util.pointZRingsMValues(plz.mRange, plz.lines)
        PolyLine(plz.bbox, pointsXY) :: zVals :: mVals :: HNil
      }
    )

  val polygonZ: Codec[PolygonZ] = polyLineZ.xmap(
    pl => PolygonZ(pl.bbox, pl.zRange, pl.mRange, pl.lines),
    poly => PolyLineZ(poly.bbox, poly.zRange, poly.mRange, poly.rings)
  )

  val shape: Codec[ShapeRecord] = RecordHeader.codec
    .flatPrepend { header =>
      fixedSizeBytes(
        header.byteLength,
        discriminated[Shape]
          .by(int32L)
          .subcaseO(ShapeType.nullShape) {
            case _: NullShape.type => Some(NullShape)
            case _                 => None
          }(Codecs.nullShape)
          .subcaseO(ShapeType.point) {
            case p: Point => Some(p)
            case _        => None
          }(Codecs.point)
          .subcaseO(ShapeType.multiPoint) {
            case mp: MultiPoint => Some(mp)
            case _              => None
          }(Codecs.multiPoint)
          .subcaseO(ShapeType.multiPointZ) {
            case mpz: MultiPointZ => Some(mpz)
            case _                => None
          }(Codecs.multiPointZ)
          .subcaseO(ShapeType.polyLine) {
            case pl: PolyLine => Some(pl)
            case _            => None
          }(Codecs.polyLine)
          .subcaseO(ShapeType.polygon) {
            case p: Polygon => Some(p)
            case _          => None
          }(Codecs.polygon)
          .subcaseO(ShapeType.pointZ) {
            case p: PointZ => Some(p)
            case _         => None
          }(Codecs.pointZ)
          .subcaseO(ShapeType.polyLineZ) {
            case p: PolyLineZ => Some(p)
            case _            => None
          }(Codecs.polyLineZ)
          .subcaseO(ShapeType.polygonZ) {
            case p: PolygonZ => Some(p)
            case _           => None
          }(Codecs.polygonZ)
          .subcaseO(ShapeType.pointM) {
            case p: PointM => Some(p)
            case _         => None
          }(Codecs.pointM)
          .subcaseO(ShapeType.polyLineM) {
            case s: PolyLineM => Some(s)
            case _            => None
          }(Codecs.polyLineM)
          .subcaseO(ShapeType.polygonM) {
            case s: PolygonM => Some(s)
            case _           => None
          }(Codecs.polygonM)
          .subcaseO(ShapeType.multiPointM) {
            case s: MultiPointM => Some(s)
            case _              => None
          }(Codecs.multiPointM)
      ).hlist
    }
    .as[ShapeRecord]

  val shpStream: StreamDecoder[ShapeRecord] = StreamDecoder
    .many(shape)
}
