package works.worace.shp4s

sealed trait Shape
case object NullShape extends Shape
case class Point(x: Double, y: Double) extends Shape
case class PointZ(x: Double, y: Double, z: Double, m: Option[Double]) extends Shape
case class PointM(x: Double, y: Double, m: Double) extends Shape {
  def pointXY: Point = Point(x, y)
}
case class MultiPoint(bbox: BBox, points: Vector[Point]) extends Shape
case class MultiPointZ(
  bbox: BBox,
  points: Vector[Point],
  z: RangedValues,
  m: Option[RangedValues]
) extends Shape
case class MultiPointM(bbox: BBox, mRange: Range, points: Vector[PointM]) extends Shape {
  def mRangedValues: RangedValues = RangedValues(mRange.min, mRange.max, points.map(_.m))
}
case class PolyLine(bbox: BBox, lines: Vector[Vector[Point]]) extends Shape {
  def numPoints: Int = lines.map(_.size).sum
}
case class Polygon(bbox: BBox, rings: Vector[Vector[Point]]) extends Shape
case class PolyLineZ(bbox: BBox, zRange: Range, mRange: Option[Range], lines: Vector[Vector[PointZ]]) extends Shape
case class PolygonZ(bbox: BBox, zRange: Range, mRange: Option[Range], rings: Vector[Vector[PointZ]]) extends Shape

object PointZ {
  val MIN_SHP_DOUBLE: Double = -1.0E38
  def apply(x: Double, y: Double, z: Double, m: Double): PointZ = {
    PointZ(x, y, z, Some(m).filter(_ > MIN_SHP_DOUBLE))
  }
}
