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
      header <- FileHeader.decode(file.bitvec)
      recordHeader <- RecordHeader.decode(header.remainder)
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
    assertEquals(shapeTypes, Set("works.worace.shp4s.Point"))
  }

  test("MultiPointZ") {
    shapeTest(TestFiles.multiPointZ, ShapeType.multiPointZ, MultiPointZ.codec) { mpz =>
      val bb = BBox(431478.25, 141891.97, 431478.25, 141891.97)
      assertEquals(mpz.bbox, bb)
      assertEquals(mpz.points.head, Point(431478.25, 141891.97))
    }
  }

  test("MultiPointZ File") {
    val mpzs = Core.readAllSync(TestFiles.multiPointZ.path)
    assertEquals(mpzs.size, 312)
    val types = mpzs.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.MultiPointZ"))
  }

  test("Polyline") {
    shapeTest(TestFiles.polyLine, ShapeType.polyLine, Codecs.polyLine) { pl =>
      val bb = BBox(-118.48595907794942, 29.394034927600217, -81.68213083231271, 34.08730621013162)
      assertEquals(pl.bbox, bb)
      assertEquals(pl.lines.head.head, Point(-118.48595907794942, 34.01473938049082))
    }
  }

  test("polyline file") {
    val pls = Core.readAllSync(TestFiles.polyLine.path)
    assertEquals(pls.size, 233)
    val types = pls.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.PolyLine"))
  }

  test("Polygon") {
    shapeTest(TestFiles.polygon, ShapeType.polygon, Codecs.polygon) { shp =>
      val bbox = shp.bbox
      assertEquals(bbox, BBox(-20.0, -90.0, -10.0, -60.0))
      assertEquals(shp.rings.head.head, Point(-20.0, -60.0))
    }
  }

  test("Polygon file") {
    val pls = Core.readAllSync(TestFiles.polygon.path)
    assertEquals(pls.size, 10)
    val types = pls.map(_.shape.getClass.getName).toSet
    assertEquals(types, Set("works.worace.shp4s.Polygon"))
  }

  test("MultiPoint") {
    shapeTest(TestFiles.multiPoint, ShapeType.multiPoint, Codecs.multiPoint) { shp =>
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
    shapeTest(TestFiles.pointZ, ShapeType.pointZ, PointZ.codec) { shp =>
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

  test("PointZ with M") {
    val pzs = Core.readAllSync(TestFiles.pointZM.path)
    assertEquals(2, pzs.size)
    val exp = Vector(
      Feature(
        1,
        PointZ(-118.13245921166909, 33.855167385820714, 15.0, Some(22.0)),
        Map("id" -> DBFNumeric(1), "example" -> DBFString("pizza"))
      ),
      Feature(
        2,
        PointZ(-118.13236902128844, 33.85485540853966, 12.0, Some(3.0)),
        Map("id" -> DBFNumeric(2), "example" -> DBFString("calzone"))
      )
    )
    assertEquals(pzs, exp)
  }

  test("PointM") {
    val pms = Core.readAllSync(TestFiles.pointM.path)
    assertEquals(2, pms.size)
    val exp = Vector(
      Feature(
        1,
        PointM(-118.13297289981072, 33.85510295853156, 123.0),
        Map("id" -> DBFNumeric(1), "test" -> DBFString("asdf"))
      ),
      Feature(
        2,
        PointM(-118.1324465853103, 33.85394911703719, 456.0),
        Map("id" -> DBFNumeric(2), "test" -> DBFString("zxcb"))
      )
    )
    assertEquals(pms, exp)
  }

  test("polygonz") {
    shapeTest(TestFiles.polygonZ, ShapeType.polygonZ, Codecs.polygonZ) { shp =>
      val bbox = BBox(-0.7965, 5.98180, -0.78958, 5.99236)
      assertBBox(shp.bbox, bbox)
      assertEquals(shp.zRange, Range(156.0, 156.0))
      assertEquals(shp.mRange, None)
      assertEquals(shp.rings.size, 1)

      val points = shp.rings.flatten.take(3)
      assertEquals(
        points,
        Vector(
          PointZ(-0.7954166666666667, 5.992083333333333, 156.0, None),
          PointZ(-0.7954166666666667, 5.992361111111111, 156.0, None),
          PointZ(-0.795138888888889, 5.992361111111111, 156.0, None)
        )
      )
    }
  }

  test("PolygonZ File") {
    val pzs = Core.readAllSync(TestFiles.polygonZ.path)
    assertEquals(pzs.size, 16)
    val ranges = pzs.map(_.shape.asInstanceOf[PolygonZ].zRange).toSet
    assert(ranges.contains(Range(19.0, 19.0)))
  }

  test("polylinez") {
    val shps = Core.readAllSync(TestFiles.polyLineZ.path)
    val plzs = shps.map(_.shape.asInstanceOf[PolyLineZ])
    assertEquals(plzs.size, 3)
    assertEquals(plzs.map(_.lines.size), Vector(1, 1, 2))

    val exp = Vector(
      PolyLineZ(
        BBox(1.0, 2.0, 2.0, 3.0),
        Range(-4.0, -3.0),
        None,
        Vector(Vector(PointZ(1.0, 2.0, -3.0, None), PointZ(2.0, 3.0, -4.0, None)))
      ),
      PolyLineZ(
        BBox(179, -22, 180, -18),
        Range(32, 34),
        None,
        Vector(
          Vector(PointZ(180, -22, 34, None), PointZ(179, -18, 32, None))
        )
      ),
      PolyLineZ(
        BBox(1, -22, 180, 3),
        Range(-4, 34),
        None,
        Vector(
          Vector(PointZ(1.0, 2.0, -3.0, None), PointZ(2.0, 3.0, -4.0, None)),
          Vector(PointZ(180, -22, 34, None), PointZ(179, -18, 32, None))
        )
      )
    )

    assertEquals(plzs, exp)
  }

  test("polylinem".only) {
    val shps = Core.readAllSync(TestFiles.polyLineM.path)
    val exp = Vector(
      Feature(
        1,
        PolyLineM(
          BBox(-118.1334591590923, 33.854545353375826, -118.13280124313243, 33.85519298939882),
          Range(1.0, 3.0),
          Vector(
            Vector(
              PointM(-118.1334591590923, 33.85519298939882, 1.0),
              PointM(-118.13280124313243, 33.85485889145045, 2.0),
              PointM(-118.1328526428168, 33.854545353375826, 3.0)
            )
          )
        ),
        Map("id" -> DBFNumeric(1), "test" -> DBFString("pizza"))
      )
    )
    assertEquals(shps, exp)
  }
}
