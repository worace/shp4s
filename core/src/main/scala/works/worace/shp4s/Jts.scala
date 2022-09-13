package works.worace.shp4s

import org.locationtech.jts.{geom => jts}
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.algorithm.Orientation

case class JtsFeature(
  geometry: Geometry,
  properties: Map[String, DBFValue]
)

object Jts {
  val DEFAULT_PRECISION_MODEl = new jts.PrecisionModel(jts.PrecisionModel.FLOATING)
  object implicits {
    implicit class ShpShapeToJts(shape: Shape) {
      def toJts(
        srid: Int = 0,
        precisionModel: jts.PrecisionModel = DEFAULT_PRECISION_MODEl
      ): Geometry =
        shapeToJts(shape, srid, precisionModel)
    }

    implicit class ShpFeatureToJts(feature: Feature) {
      def toJts(
        srid: Int = 0,
        precisionModel: jts.PrecisionModel = DEFAULT_PRECISION_MODEl
      ): JtsFeature =
        featureToJts(feature, srid, precisionModel)
    }
  }

  def featureToJts(
    feature: Feature,
    srid: Int = 0,
    precisionModel: jts.PrecisionModel = DEFAULT_PRECISION_MODEl
  ): JtsFeature = {
    JtsFeature(shapeToJts(feature.shape, srid, precisionModel), feature.properties)
  }

  def shapeToJts(
    shape: Shape,
    srid: Int = 0,
    precisionModel: jts.PrecisionModel = DEFAULT_PRECISION_MODEl
  ): Geometry = {
    val pm = precisionModel
    val gf = new jts.GeometryFactory(pm, srid)
    // new jts.Point(0, 0)
    shape match {
      case p @ Point(_, _)        => pointToJtsPoint(p, srid, pm)
      case p @ PointM(_, _, _)    => pointToJtsPoint(p, srid, pm)
      case p @ PointZ(_, _, _, _) => pointToJtsPoint(p, srid, pm)
      case MultiPoint(_, points) => {
        val pointsArr = points.toArray.map(p => pointToJtsPoint(p, srid, pm))
        gf.createMultiPoint(pointsArr)
      }
      // MultiPointZ with Z and M values
      case mpz @ MultiPointZ(_, points, _, Some(_)) => {
        val pointsArr = points
          .zip(mpz.zRangedValues.values)
          .zip(mpz.mRangedValues.get.values)
          .map {
            case ((point, z), m) =>
              gf.createPoint(new jts.CoordinateXYZM(point.x, point.y, z, m))
          }
          .toArray
        gf.createMultiPoint(pointsArr)
      }
      // MultiPointZ with only Z values
      case mpz @ MultiPointZ(_, points, _, None) => {
        val pointsArr = points
          .zip(mpz.zRangedValues.values)
          .map {
            case ((point, z)) =>
              gf.createPoint(new jts.Coordinate(point.x, point.y, z))
          }
          .toArray
        gf.createMultiPoint(pointsArr)
      }
      case MultiPointM(_, _, points) => {
        val pointsArr = points.map { p => gf.createPoint(new jts.CoordinateXYM(p.x, p.y, p.m)) }.toArray
        gf.createMultiPoint(pointsArr)
      }
      case pl @ PolyLine(_, _)        => pl.toJtsLineStringOrMultiLineString(gf)
      case pl @ PolyLineZ(_, _, _, _) => pl.toJtsLineStringOrMultiLineString(gf)
      case pl @ PolyLineM(_, _, _)    => pl.toJtsLineStringOrMultiLineString(gf)
      case p @ Polygon(_, _)          => p.toJtsPolygonOrMultiPolygon(gf)
      case p @ PolygonM(_, _, _)      => p.toJtsPolygonOrMultiPolygon(gf)
      case p @ PolygonZ(_, _, _, _)   => p.toJtsPolygonOrMultiPolygon(gf)
      case NullShape                  => gf.createEmpty(0)
    }
  }

  private implicit class PointShapeToJts(point: PointShape) {
    def toJtsCoord: jts.Coordinate = {
      point match {
        case Point(x, y)              => new jts.Coordinate(x, y)
        case PointM(x, y, m)          => new jts.CoordinateXYM(x, y, m)
        case PointZ(x, y, z, Some(m)) => new jts.CoordinateXYZM(x, y, z, m)
        case PointZ(x, y, z, None)    => new jts.Coordinate(x, y, z)
      }
    }
  }

  private implicit class PolylineShapeToJts(pl: PolyLineShape) {
    def toJtsLineStringOrMultiLineString(gf: jts.GeometryFactory): jts.Geometry = {
      val lines = pl.pointLines

      val jtsCoords = lines.toArray.map { line => line.toArray.map { p => p.toJtsCoord } }
      if (jtsCoords.size > 1) {
        gf.createLineString(jtsCoords.head)
      } else {
        gf.createMultiLineString(jtsCoords.map(gf.createLineString))
      }
    }
  }

  private implicit class PolygonShapeToJts(polygon: PolygonShape) {
    def toJtsPolygonOrMultiPolygon(gf: jts.GeometryFactory): jts.Geometry = {
      val rings = polygon.pointShapes
      // https://gis.stackexchange.com/questions/122816/shapefiles-polygon-type-is-it-in-fact-multipolygon
      // https://gis.stackexchange.com/questions/225368/understanding-difference-between-polygon-and-multipolygon-for-shapefiles-in-qgis
      // shapefile polygon decoding....
      // * 0 rings - polygon (empty)
      // * 1 ring - polygon
      // * multiple rings -- depends on winding order
      //   CW: Start outer ring
      //   --- successive CCW -- inner rings
      //   CW / CCW / CCW --> Polygon 2 holes
      //   CW / CCW / CW --> MultiPolygon: 1xPolygon with 1 hole, 1x outer ring polygon
      if (polygon.pointShapes.isEmpty) {
        gf.createPolygon
      } else if (rings.size == 1) {
        gf.createPolygon(rings.head.toArray.map(_.toJtsCoord))
      } else {
        val coordSeqs: Array[Array[jts.Coordinate]] =
          rings.toArray.map(_.toArray.map(_.toJtsCoord))

        // Clockwise rings indicate exteriors
        val cwIndexes = (0 until rings.size).filter { idx => !Orientation.isCCW(coordSeqs(idx)) }

        if (cwIndexes.size == 1) {
          // Single outer ring, make polygon
          val Array(head, rest @ _*) = coordSeqs.map(gf.createLinearRing)
          gf.createPolygon(head, rest.toArray)
        } else {
          // multiple outer rings, make multipolygon
          val startEnds = cwIndexes.zip(cwIndexes.drop(1) :+ coordSeqs.size).toArray
          val polys: Array[jts.Polygon] = startEnds.map {
            case (start, stop) =>
              val outer = gf.createLinearRing(coordSeqs(start))
              val inners = coordSeqs.slice(start + 1, stop - 1).map(gf.createLinearRing(_))
              gf.createPolygon(outer, inners)
          }
          gf.createMultiPolygon(polys)
        }
      }
    }
  }

  private def pointToJtsPoint(point: PointShape, srid: Int, pm: jts.PrecisionModel): jts.Point = {
    val gf = new jts.GeometryFactory(pm, srid)
    gf.createPoint(point.toJtsCoord)
  }

  new jts.PrecisionModel(jts.PrecisionModel.FLOATING)
}
