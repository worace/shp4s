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
