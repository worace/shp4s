package works.worace.shp4s

import scodec._
import scodec.codecs._

private object Util {
  def sliceIndices[_](vec: Vector[Vector[_]]): Vector[(Int, Int)] = {
    var start: Int = 0
    vec.map { slice =>
      val indices = (start, start + slice.size)
      start += slice.size
      indices
    }
  }

  def zippedWithFlatVecTriple[A, B, C, D](
    vec: Vector[Vector[A]],
    flatB: Vector[B],
    flatC: Vector[C]
  )(
    handler: (A, Option[B], Option[C]) => D
  ): Vector[Vector[D]] = {
    var index = 0

    vec.map { inner =>
      inner.map { element =>
        val b = flatB.lift(index)
        val c = flatC.lift(index)
        index += 1
        handler(element, b, c)
      }
    }
  }

  def zippedWithFlatVec[A, B, C](vec: Vector[Vector[A]], flat: Vector[B])(
    h: (A, B) => C
  ): Vector[Vector[C]] = {
    vec.zip(sliceIndices(vec)).map {
      case (slice, (start, stop)) =>
        val leftSlice = flat.slice(start, stop)
        slice.zip(leftSlice).map { case (a, b) => h(a, b) }
    }
  }

  def ringRangedValues[P](range: Range, rings: Vector[Vector[P]])(
    getter: P => Double
  ): RangedValues = {
    val values = rings.flatMap(ring => ring.map(p => getter(p)))
    RangedValues(range.min, range.max, values)
  }

  def pointZRingsZValues(range: Range, rings: Vector[Vector[PointZ]]): RangedValues = {
    val zVals = rings.flatMap(line => line.map(_.z))
    RangedValues(range.min, range.max, zVals)
  }

  def pointZRingsMValues(
    range: Option[Range],
    rings: Vector[Vector[PointZ]]
  ): Option[RangedValues] = {
    range.map { range =>
      val mVals = rings.flatMap(line => line.map(_.m.getOrElse(0.0)))
      RangedValues(range.min, range.max, mVals)
    }
  }

  def offsetSlices[T](points: Vector[T], offsets: Vector[Int]): Vector[Vector[T]] = {
    if (offsets.size > 1) {
      val slices = offsets ++ Vector(points.size)

      slices
        .sliding(2)
        .map { case Vector(start, finish) => points.slice(start, finish) }
        .toVector
    } else {
      Vector(points)
    }
  }

  def ifAvailable[A](codec: Codec[A], zero: A): Codec[Option[A]] = {
    optional(lookahead(codec.unit(zero)), codec)
  }

  def rangedValues(num: Int) =
    (doubleL :: doubleL :: vectorOfN(provide(num), doubleL)).as[RangedValues]
}
