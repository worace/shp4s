package works.worace.shp4s

import Core.{Range, RangedValues, PointZ}

private object Util {
  def zippedWithFlatVec[A, B, C](
    vec: Vector[Vector[A]],
    flat: Vector[B])(
    h: (A, B) => C
  ): Vector[Vector[C]] = {
    val sliceIndices = vec
      .map(_.size)
      .map(_ - 1)
      .prepended(0)
      .sliding(2)
      .toVector

    vec.zip(sliceIndices).map { case (slice, Vector(start, stop)) =>
      val leftSlice = flat.slice(start, stop)
      slice.zip(leftSlice).map { case (a, b) => h(a, b) }
    }
  }

  def pointZRingsZValues(range: Range, rings: Vector[Vector[PointZ]]): RangedValues = {
    val zVals = rings.flatMap(line => line.map(_.z))
    RangedValues(range.min, range.max, zVals)
  }

  def pointZRingsMValues(range: Option[Range], rings: Vector[Vector[PointZ]]): Option[RangedValues] = {
    range.map { range =>
      val mVals = rings.flatMap(line => line.map(_.m.getOrElse(0.0)))
      RangedValues(range.min, range.max, mVals)
    }
  }
}
