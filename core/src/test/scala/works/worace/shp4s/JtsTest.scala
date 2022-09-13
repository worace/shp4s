package works.worace.shp4s

import org.locationtech.jts.{geom => jts}

class JtsTest extends munit.FunSuite {
  test("converting polygon") {
    val shps = Core.readAllSyncFromPath(TestFiles.polygonZM.path)
    val exp = Vector(
      Feature(
        1,
        PolygonZ(
          BBox(-118.13306035523306, 33.85514373848763, -118.1326517388282, 33.85539412471017),
          Range(1.0, 4.0),
          Some(Range(5.0, 8.0)),
          Vector(
            Vector(
              PointZ(-118.13306035523306, 33.85531587901563, 4.0, Some(7.0)),
              PointZ(-118.13295602764032, 33.85539412471017, 1.0, Some(5.0)),
              PointZ(-118.1326517388282, 33.85529153591066, 2.0, Some(6.0)),
              PointZ(-118.13300123626384, 33.85514373848763, 3.0, Some(8.0)),
              PointZ(-118.13306035523306, 33.85531587901563, 4.0, Some(7.0))
            )
          )
        ),
        Map("id" -> DBFNumeric(1))
      )
    )
    val converted = Jts.featureToJts(shps.head)
    assertEquals(converted.geometry.getCoordinates().head.x, -118.13306035523306)
    assertEquals(converted.geometry.getCoordinates().head.y, 33.85531587901563)
    assertEquals(converted.geometry.getCoordinates().head.z, 4.0)
    assertEquals(converted.geometry.getCoordinates().head.getM(), 7.0)
    assertEquals(converted.properties("id"), DBFNumeric(1))
  }

  test("multipolygon") {
    val shps = Core.readAllSyncFromPath(TestFiles.gdalGjMultiPoly.path)
    assertEquals(shps.size, 1)
    assertEquals(shps.head.shape.getClass.getName, "works.worace.shp4s.Polygon")
    val poly = shps.head.shape.asInstanceOf[Polygon]
    assertEquals(poly.rings.size, 3)
    val converted = Jts.shapeToJts(poly)
    assertEquals(converted.getGeometryType, "MultiPolygon")
    val mp = converted.asInstanceOf[jts.MultiPolygon]
    assertEquals(mp.getNumGeometries, 3)
  }

  test("mixed polygons and multi polygons - multiple holes and shells") {
    val shps = Core.readAllSyncFromPath(TestFiles.portlandZips.path)
    assertEquals(shps.size, 142)
    assertEquals(shps.head.shape.getClass.getName, "works.worace.shp4s.Polygon")
    val polys = shps.map(_.shape.asInstanceOf[Polygon])

    val ringCounts = polys.map(_.rings.size).toSet
    assertEquals(ringCounts, Set(1,2,3,4))

    val single = polys.find(_.rings.size == 1).get
    val singleConv = Jts.shapeToJts(single)
    assertEquals(singleConv.getGeometryType, "Polygon")

    val quad = polys.find(_.rings.size == 4).get
    println(quad)
    val quadConv = Jts.shapeToJts(quad)
    assertEquals(quadConv.getGeometryType, "MultiPolygon")
    assertEquals(quadConv.asInstanceOf[jts.MultiPolygon].getNumGeometries, 2)
    val poly1 = quadConv.asInstanceOf[jts.MultiPolygon].getGeometryN(0)
    val poly2 = quadConv.asInstanceOf[jts.MultiPolygon].getGeometryN(1)
    println(poly1)
    println(poly2)
    val quadIdx = polys.indexWhere(_.rings.size == 4)
    println(shps(quadIdx))

  }
}
