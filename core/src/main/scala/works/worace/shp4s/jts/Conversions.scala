package works.worace.shp4s.jts

import works.worace.shp4s._
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.{geom => jts}

object Conversions {
  def toJts(shape: Shape, srid: Int, pm: jts.PrecisionModel): Geometry = {
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
      case MultiPointZ(_, points, zs, Some(m)) => {
        val pointsArr = points
          .zip(zs.values)
          .zip(m.values)
          .map {
            case ((point, z), m) =>
              gf.createPoint(new jts.CoordinateXYZM(point.x, point.y, z, m))
          }
          .toArray
        gf.createMultiPoint(pointsArr)
      }
      case MultiPointZ(_, points, zs, None) => {
        val pointsArr = points
          .zip(zs.values)
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
      case PolyLine(_, lines) => {
        if (lines.size > 1) {
          //multilinestring
          val linestrings = lines.toArray.map(l => gf.createLineString(l.toArray.map(_.toJtsCoord)))
          gf.createMultiLineString(linestrings)
        } else {
          //linestring
          gf.createLineString(lines.head.toArray.map(_.toJtsCoord))
        }
      }
      case _ => throw new RuntimeException("oops")
    }
  }

  def toJts(shape: Shape): Geometry = {
    toJts(shape, 0, defaultPM)
  }

  implicit class PointShapeToJts(point: PointShape) {
    def toJtsCoord: jts.Coordinate = {
      point match {
        case Point(x, y)              => new jts.Coordinate(x, y)
        case PointM(x, y, m)          => new jts.CoordinateXYM(x, y, m)
        case PointZ(x, y, z, Some(m)) => new jts.CoordinateXYZM(x, y, z, m)
        case PointZ(x, y, z, None)    => new jts.Coordinate(x, y, z)
      }
    }
  }

  private def pointToJtsPoint(point: PointShape, srid: Int, pm: jts.PrecisionModel): jts.Point = {
    val gf = new jts.GeometryFactory(pm, srid)
    gf.createPoint(point.toJtsCoord)
    // point match {
    //   case Point(x, y)              => gf.createPoint(new jts.Coordinate(x, y))
    //   case PointM(x, y, m)          => gf.createPoint(new jts.CoordinateXYM(x, y, m))
    //   case PointZ(x, y, z, Some(m)) => gf.createPoint(new jts.CoordinateXYZM(x, y, z, m))
    //   case PointZ(x, y, z, None)    => gf.createPoint(new jts.Coordinate(x, y, z))
    // }
  }

  private val defaultPM = new jts.PrecisionModel(jts.PrecisionModel.FLOATING)
}
