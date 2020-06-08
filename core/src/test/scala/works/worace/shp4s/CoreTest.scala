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
import works.worace.shp4s.Core.Shape
import scodec.Codec
import works.worace.shp4s.Core.Point
import works.worace.shp4s.Core.PointZ
import works.worace.shp4s.Core.MultiPoint
import Core.ShpCodecs

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
  val polygon = Resource("ne_10m_admin_0_antarctic_claims.shp")
  val multiPoint = Resource("multipoint.shp")
  val pointZ = Resource("pointz.shp")
}

class CoreTest extends munit.FunSuite {
  implicit val csIO: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  def assertInDelta(a: Double, b: Double, delta: Double): Unit = {
    assert((a - b).abs < delta, s"Expected $a within $delta of $b")
  }

  def shapeTest[S <: Shape](file: Resource, discrim: Int, decoder: Codec[S])(
    test: S => Unit
  ): Unit = {
    val t = for {
      header <- Core.header.decode(file.bitvec)
      recordHeader <- Core.recordHeader.decode(header.remainder)
      discriminator <- scodec.codecs.int32L.decode(recordHeader.remainder)
      shape <- scodec.codecs.fixedSizeBits(recordHeader.value.bitLength, decoder).decode(discriminator.remainder)
    } yield {
      test(shape.value)
    }
    t.toOption.getOrElse(fail(s"Expected succesful decoder. Got $t"))
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
    shapeTest(TestFiles.multiPointZ, ShapeType.multiPointZ, ShpCodecs.multiPointZ) { mpz =>
      val bb = BBox(431478.25, 141891.97, 431478.25, 141891.97)
      assertEquals(mpz.bbox, bb)
      assertEquals(mpz.points.head, Point(431478.25, 141891.97))
    }
  }

  test("MultiPointZ File") {
    val mpzs = Core.readAllSync(TestFiles.multiPointZ.path)
    assertEquals(mpzs.size, 312)
    val types = mpzs.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Core$MultiPointZ"))
  }

  test("Polyline") {
    shapeTest(TestFiles.polyline, ShapeType.polyline, ShpCodecs.polyline) { pl =>
      val bb = BBox(-118.48595907794942, 29.394034927600217, -81.68213083231271, 34.08730621013162)
      assertEquals(pl.bbox, bb)
      assertEquals(pl.lines.head.head, Point(-118.48595907794942, 34.01473938049082))
    }
  }

  test("polyline file") {
    val pls = Core.readAllSync(TestFiles.polyline.path)
    assertEquals(pls.size, 233)
    val types = pls.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Core$PolyLine"))
  }

  test("Polygon") {
    shapeTest(TestFiles.polygon, ShapeType.polygon, ShpCodecs.polygon) { shp =>
      val bbox = shp.bbox
      assertEquals(bbox, BBox(-20.0, -90.0, -10.0, -60.0))
      assertEquals(shp.rings.head.head, Point(-20.0, -60.0))
    }
  }

  test("Polygon file") {
    val pls = Core.readAllSync(TestFiles.polygon.path)
    assertEquals(pls.size, 10)
    val types = pls.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Core$Polygon"))
  }

  test("MultiPoint") {
    shapeTest(TestFiles.multiPoint, ShapeType.multiPoint, ShpCodecs.multiPoint) { shp =>
      val bbox = shp.bbox
      assertEquals(bbox, BBox(-123.0, -20.0, 10.0, 47.5234523))
      assertEquals(shp.points, Vector(Point(10.0, -20.0), Point(-123.0, 47.5234523)))
    }
  }

  test("MultiPoint File") {
    val mps = Core.readAllSync(TestFiles.multiPoint.path)
    assertEquals(mps.size, 2)

    val exp = Vector(
      MultiPoint(
        BBox(-123.0, -20.0, 10.0, 47.5234523),
        Vector(Point(10.0, -20.0), Point(-123.0, 47.5234523))
      ),
      MultiPoint(
        BBox(-83.0, -20.23, 145.0, 24.0293),
        Vector(Point(5.2, -7.0), Point(-83.0, 24.0293), Point(145.0, -20.23))
      )
    )
    assertEquals(mps.map(_.shape), exp)
  }

  test("PointZ") {
    shapeTest(TestFiles.pointZ, ShapeType.pointZ, ShpCodecs.pointZ) { shp =>
      assertEquals(shp, PointZ(1.0, 2.0, -3.0, None))
    }
  }

  test("PointZ File") {
    val pzs = Core.readAllSync(TestFiles.pointZ.path)
    assertEquals(pzs.size, 2)
    assertEquals(
      pzs.map(_.shape),
      Vector(
        PointZ(1.0, 2.0, -3.0, None),
        PointZ(180.0, -22.0, 34.0, None)
      )
    )
  }
}
