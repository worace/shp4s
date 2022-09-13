package works.worace.shp4s

sealed trait PointShape
sealed trait MultiPointShape

sealed trait PolyLineShape {
  def pointLines: Vector[Vector[PointShape]]
}

sealed trait PolygonShape {
  def pointShapes: Vector[Vector[PointShape]]
}

sealed trait Shape
case object NullShape extends Shape
case class Point(x: Double, y: Double) extends Shape with PointShape
case class PointZ(x: Double, y: Double, z: Double, m: Option[Double]) extends Shape with PointShape
case class PointM(x: Double, y: Double, m: Double) extends Shape with PointShape {
  def pointXY: Point = Point(x, y)
}
case class MultiPoint(bbox: BBox, points: Vector[Point]) extends Shape with MultiPointShape
case class MultiPointZ(
  bbox: BBox,
  points: Vector[Point],
  z: RangedValues,
  m: Option[RangedValues]
) extends Shape
    with MultiPointShape
case class MultiPointM(bbox: BBox, mRange: Range, points: Vector[PointM])
    extends Shape
    with MultiPointShape {
  def mRangedValues: RangedValues = RangedValues(mRange.min, mRange.max, points.map(_.m))
}
case class PolyLine(bbox: BBox, lines: Vector[Vector[Point]]) extends Shape with PolyLineShape {
  def numPoints: Int = lines.map(_.size).sum
  def pointLines = lines
}
case class PolyLineZ(
  bbox: BBox,
  zRange: Range,
  mRange: Option[Range],
  lines: Vector[Vector[PointZ]]
) extends Shape
    with PolyLineShape {
  def pointLines = lines
}
case class PolyLineM(bbox: BBox, mRange: Range, lines: Vector[Vector[PointM]])
    extends Shape
    with PolyLineShape {
  def pointLines = lines
}
case class Polygon(bbox: BBox, rings: Vector[Vector[Point]]) extends Shape with PolygonShape {
  def pointShapes = rings
}
case class PolygonZ(bbox: BBox, zRange: Range, mRange: Option[Range], rings: Vector[Vector[PointZ]])
    extends Shape
    with PolygonShape {
  def pointShapes = rings
}
case class PolygonM(bbox: BBox, mRange: Range, rings: Vector[Vector[PointM]])
    extends Shape
    with PolygonShape {
  def pointShapes = rings
}

object NullShapeCodec extends ShpCodec[NullShape.type] {
  val codec = Codecs.nullShape
}

object Point extends ShpCodec[Point] {
  val codec = Codecs.point
}

object PointZ extends ShpCodec[PointZ] {
  val MIN_SHP_DOUBLE: Double = -1.0e38
  def apply(x: Double, y: Double, z: Double, m: Double): PointZ = {
    PointZ(x, y, z, Some(m).filter(_ > MIN_SHP_DOUBLE))
  }
  val codec = Codecs.pointZ
}

object PointM extends ShpCodec[PointM] {
  val codec = Codecs.pointM
}

object PolyLine extends ShpCodec[PolyLine] {
  val codec = Codecs.polyLine
}

object PolyLineZ extends ShpCodec[PolyLineZ] {
  val codec = Codecs.polyLineZ
}

// object PolyLineM extends ShpCodec[PolyLineM] {
//   val codec = Codecs.polyLineM
// }

object Polygon extends ShpCodec[Polygon] {
  val codec = Codecs.polygon
}

object PolygonZ extends ShpCodec[PolygonZ] {
  val codec = Codecs.polygonZ
}

// object PolygonM extends ShpCodec[PolygonM] {
//   val codec = Codecs.polyLineM
// }

object MultiPoint extends ShpCodec[MultiPoint] {
  val codec = Codecs.multiPoint
}

object MultiPointZ extends ShpCodec[MultiPointZ] {
  val codec = Codecs.multiPointZ
}

object MultiPointM extends ShpCodec[MultiPointM] {
  val codec = Codecs.multiPointM
}

// object MultiPointZ extends ShpCodec[MultiPointZ] {
//   val codec = Codecs.multiPointZ
// }
