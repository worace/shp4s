package works.worace.shp4s

import cats.effect.{IO, ContextShift}
import scodec.Codec
import works.worace.shp4s.Core._

class CoreTest extends munit.FunSuite {
  implicit val csIO: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  def assertInDelta(a: Double, b: Double, delta: Double): Unit = {
    assert((a - b).abs < delta, s"Expected $a within $delta of $b")
  }

  def assertBBox(a: BBox, b: BBox): Unit = {
    assertInDelta(a.xMin, b.xMin, 0.0001)
    assertInDelta(a.yMin, b.yMin, 0.0001)
    assertInDelta(a.xMax, b.xMax, 0.0001)
    assertInDelta(a.yMax, b.yMax, 0.0001)
  }

  def shapeTest[S <: Shape](file: Resource, discrim: Int, decoder: Codec[S])(
    test: S => Unit
  ): Unit = {
    val t = for {
      header <- Core.header.decode(file.bitvec)
      recordHeader <- Core.recordHeader.decode(header.remainder)
      discriminator <- scodec.codecs.int32L.decode(recordHeader.remainder)
      shape <- scodec.codecs
        .fixedSizeBits(recordHeader.value.bitLength, decoder)
        .decode(discriminator.remainder)
    } yield {
      test(shape.value)
    }
    t.toOption.getOrElse(fail(s"Expected succesful decoder. Got $t"))
  }

  test("reading a header") {
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
    shapeTest(TestFiles.polyLine, ShapeType.polyLine, ShpCodecs.polyLine) { pl =>
      val bb = BBox(-118.48595907794942, 29.394034927600217, -81.68213083231271, 34.08730621013162)
      assertEquals(pl.bbox, bb)
      assertEquals(pl.lines.head.head, Point(-118.48595907794942, 34.01473938049082))
    }
  }

  test("polyline file") {
    val pls = Core.readAllSync(TestFiles.polyLine.path)
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

//   test("PolyLineZ") {
// -    xMin = -0.7965277777777777,
// -    yMin = 5.981805555555556,
// -    xMax = -0.7895833333333334,
// -    yMax = 5.992361111111111
// +    xMin = -123.0,
// +    yMin = -20.0,
// +    xMax = 10.0,
// +    yMax = 47.5234523
//    ),
//    zRange = Range(
// -    min = 156.0,
// -    max = 156.0
// +    min = 0.0,
// +    max = 0.0
//    ),

  // shapeTest(TestFiles.polyLineZ, ShapeType.polyLineZ, ShpCodecs.polyLineZ) { shp =>
    // val hTpz = Core.readHeader(TestFiles.polyLineZ.bytes)
    // println(hTpz.map(_.shapeType))

  //   // assertEquals(shp, PolyLineZ(bbox, Range(0.0, 0.0), None, lines))
  // }

  test("PolygonZ File") {
    val pzs = Core.readAllSync(TestFiles.polygonZ.path)
    assertEquals(pzs.size, 16)
    val pz1 = pzs.head.shape
    val bbox = BBox(-0.7965, 5.98180, -0.78958, 5.99236)
  //   val lines = Vector(Vector(PointZ(1.0, 2.0, -3.0, None)))
  //   // println(shp)
    assertBBox(pz1.bbox, bbox)
    // assertEquals(
    //   PolygonZ(),
    //   Vector(
    //     PointZ(1.0, 2.0, -3.0, None),
    //     PointZ(180.0, -22.0, 34.0, None)
    //   )
    // )
  }
}
