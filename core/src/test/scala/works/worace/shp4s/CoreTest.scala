package works.worace.shp4s

import scala.io.Source
import java.nio.file.{Files, Paths}
import cats.effect.{IO, ContextShift}
import java.net.URL
import java.nio.file.Path
import scodec.bits.BitVector
import works.worace.shp4s.Core.MultiPointZ
import works.worace.shp4s.Core.ShapeType
import works.worace.shp4s.Core.BBox

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
    val t = for {
      header <- Core.header.decode(TestFiles.multiPointZ.bitvec)
      recordHeader <- Core.recordHeader.decode(header.remainder)
      discriminator <- scodec.codecs.int32L.decode(recordHeader.remainder)
      mpz <- Core.polyline.decode(discriminator.remainder)
    } yield {
      assertEquals(header.value.shapeType, ShapeType.multiPointZ)
      assertEquals(recordHeader.value.wordsLength, 40)
      assertEquals(discriminator.value, ShapeType.multiPointZ)
      val bbox = mpz.value.bbox
      assertEquals(bbox, BBox(431478.25,141891.97,431478.25,141891.97))
    }
    t.toOption.get
  }

  test("MultiPointZ File") {
    val mpzs = Core.readAllSync(TestFiles.multiPointZ.path)
    assertEquals(mpzs.size, 312)
    val types = mpzs.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Core$MultiPointZ"))
  }

  test("Polyline") {
    val t = for {
      header <- Core.header.decode(TestFiles.polyline.bitvec)
      recordHeader <- Core.recordHeader.decode(header.remainder)
      discriminator <- scodec.codecs.int32L.decode(recordHeader.remainder)
      polyline <- Core.polyline.decode(discriminator.remainder)
    } yield {
      assertEquals(header.value.shapeType, ShapeType.polyline)
      assertEquals(recordHeader.value.wordsLength, 12268)
      assertEquals(discriminator.value, ShapeType.polyline)
      val bbox = polyline.value.bbox
      assertInDelta(bbox.xMin, -118.486, 0.01)
      assertInDelta(bbox.yMin, 29.39, 0.01)
      assertInDelta(bbox.xMax, -81.68, 0.01)
      assertInDelta(bbox.yMax, 34.0873, 0.01)
    }
    t.toOption.get
  }

  test("polyline file") {
    val pls = Core.readAllSync(TestFiles.polyline.path)
    assertEquals(pls.size, 233)
    val types = pls.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Core$PolyLine"))
  }
}
