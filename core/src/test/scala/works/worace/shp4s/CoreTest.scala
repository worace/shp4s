package works.worace.shp4s

import scala.io.Source
import java.nio.file.{Files, Paths}
import cats.effect.{IO, ContextShift}
import java.net.URL
import java.nio.file.Path
import scodec.bits.BitVector
import works.worace.shp4s.Core.MultiPointZ
import works.worace.shp4s.Core.ShapeType

case class Resource(url: URL) {
  def bytes: Array[Byte] = Files.readAllBytes(Paths.get(url.getPath))
  def path: Path = Paths.get(url.getPath)
  def bitvec: BitVector = BitVector(bytes)
}
object Resource {
  def apply(name: String): Resource = Resource(getClass.getResource(s"/$name"))
}

object TestFiles {
  val points = Resource("world-cities.shp")
  val multiPointZ = Resource("multipointZ.shp")
  val polyline = Resource("usa-major-highways.shp")
}

class CoreTest extends munit.FunSuite {
  implicit val csIO: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  def assertInDelta(a: Double, b: Double, delta: Double): Unit = {
    assert((a - b).abs < delta, s"Expected $a within $delta of $b")
  }

  test("reading a header") {
    println(TestFiles.points.path)
    val hT = Core.readHeader(TestFiles.points.bytes)
    assert(hT.isSuccess)
    val h = hT.get
    assertEquals(h.length, 35596)
    assertEquals(h.shapeType, ShapeType.point)
    assertInDelta(h.xMin, -176.15156, 0.001)
    assertInDelta(h.xMax, 179.22188, 0.001)
    assertInDelta(h.yMin, -54.792, 0.001)
    assertInDelta(h.yMax, 78.2, 0.001)
    assertEquals(h.zMin, 0.0)
    assertEquals(h.zMax, 0.0)
    assertEquals(h.mMin, 0.0)
    assertEquals(h.mMax, 0.0)
  }

  test("streaming points") {
    val points = Core.readAllSync(TestFiles.points.path)
    assertEquals(points.size, 2539)
    val shapeTypes = points.map(_.shape).map(_.getClass.getName()).toSet
    assertEquals(shapeTypes, Set("works.worace.shp4s.Core$Point"))
  }

  test("MultiPointZ") {
    import scodec.bits._
    import scodec._
    import scodec.codecs._
    val bv = TestFiles.multiPointZ.bitvec
    val h = Core.header.decode(bv).getOrElse(throw new RuntimeException)
    val rech = Core.recordHeader.decode(h.remainder).getOrElse(throw new RuntimeException)

    // for {
    //   shptype <- int32L.decode(rech.remainder)
    //   body <- fixedSizeBytes(rech.value.byteLength, Core.multiPointZ).decode(shptype.remainder)
    //   // bbox <- Core.bbox.decode(shptype.remainder)
    //   // points <- vectorOfN(int32L, Core.point).decode(bbox.remainder)
    //   // zrange <- Core.zRange.decode(points.remainder)
    //   // zvals <- doubleL.decode(zrange.remainder)
    //   // mrange <- Core.mRange.decode(zvals.remainder)
    //   // numPoints <- int32L.decode(bbox.remainder)
    //   // point <- Core.point.decode(numPoints.remainder)
    // } yield {
    //   println(body)
    //   println(body.value.bbox)
    //   println(body.value.points)
    //   println(body.value.z)
    //   println(body.value.m)
    //   // println(s"shptyp: $shptype")
    //   // println(s"bbox: $bbox")
    //   // println(s"points: $points")
    //   // println(s"zrange: $zrange")
    //   // println(s"zvals: $zvals")

    //   // NOT THERE
    //   // println(s"mrange: $mrange")
    //   // println(s"numPoints: $numPoints")
    //   // println(s"point: $point")
    // }

    // // val shapetype = int32L.decode(rech.remainder)
    // // println(shapetype)
    // // val mpz = Core.multiPointZ.decode(rech.remainder)
    // // println(mpz)

    val mpzs = Core.readAllSync(TestFiles.multiPointZ.path)
    assertEquals(mpzs.size, 312)
    val types = mpzs.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Core$MultiPointZ"))
    // Core.readHeader(TestFiles.multiPoints.bytes)
  }

  test("Polyline") {
    import scodec.bits._
    import scodec._
    import scodec.codecs._

    val bv = TestFiles.polyline.bitvec
    val h = Core.header.decode(bv).toOption.get
    assertEquals(h.value.shapeType, ShapeType.polyline)
    val rech = Core.recordHeader.decode(h.remainder).toOption.get
    val discrim = scodec.codecs.int32L.decode(rech.remainder).toOption.get
    assertEquals(discrim.value, ShapeType.polyline)

    println("rec header")
    println(rech)
    val polylineData = discrim.remainder.take(rech.value.bitLength)

    val plAtt = Core.polylineParts.decode(polylineData)
    assert(plAtt.isSuccessful)
    val pl = plAtt.toOption.get.value
    assertEquals(pl.numParts, 3)
    assertEquals(pl.numPoints, 1530)
    assertEquals(pl.partOffsets, Vector(0, 35, 942))

    for {
      bbox <- Core.bbox.decode(polylineData)
      numParts <- scodec.codecs.int32L.decode(bbox.remainder)
      numPoints <- scodec.codecs.int32L.decode(numParts.remainder)
      partOffsets <- scodec.codecs
        .vectorOfN(provide(numParts.value), int32L)
        .decode(numPoints.remainder)
      pointone <- Core.point.decode(partOffsets.remainder)
      pointtwo <- Core.point.decode(pointone.remainder)
      pointthree <- Core.point.decode(pointtwo.remainder)
      point4 <- Core.point.decode(pointthree.remainder)
      point5 <- Core.point.decode(point4.remainder)
      point6 <- Core.point.decode(point5.remainder)
      point7 <- Core.point.decode(point5.remainder)
    } yield {
      val pointData = partOffsets.remainder
      // val numPoints = 1530
      // (1 to 763).foldLeft(pointData) { (rem, i) =>
      //   println(s"point i: $i")
      //   val res = Core.point.decode(rem)
      //   println(res)
      //   res.toOption.get.remainder
      // }
      // println(bbox)
      // println(numParts)
      // println("NUM POINTS")
      // println(numPoints)
      // println(partOffsets)
      // println(pointone)
      // println(pointtwo)
      // println(pointthree)
      // println(point4)
      // println(point5)
      // println(point6)
      // println(point7)
    }


    val plfullAtt = Core.polyline.decode(polylineData)
    println("fullatt")
    println(plfullAtt.isSuccessful)
    println(plfullAtt)
    // val pointVec = plfullAtt.toOption.get.value._2
    // println(pointVec.slice(0, 34))
    // println(pointVec.slice(35, 941))
    // println(pointVec.size)
    // println(pointVec.slice(942, pointVec.size - 1))
  }

  test("polyline file") {
    val pls = Core.readAllSync(TestFiles.polyline.path)
    assertEquals(pls.size, 233)
    val types = pls.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Core$PolyLine"))
  }
}
