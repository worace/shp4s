package works.worace.shp4s

import scodec.bits.BitVector
import java.nio.file.{Files, Paths}
import java.net.URL
import java.nio.file.Path
import scala.io.Source

case class Resource(url: URL) {
  def bytes: Array[Byte] = Files.readAllBytes(Paths.get(url.getPath))
  def path: Path = Paths.get(url.getPath)
  def bitvec: BitVector = BitVector(bytes)
}
object Resource {
  def apply(name: String): Resource = Resource(getClass.getResource(s"/$name"))
}

object TestFiles {
  val dbf = Resource("ne_10m_admin_0_antarctic_claims.dbf")
  val points = Resource("world-cities.shp")
  val multiPointZ = Resource("multipointZ.shp")
  val polyLine = Resource("usa-major-highways.shp")
  val polygon = Resource("ne_10m_admin_0_antarctic_claims.shp")
  val multiPoint = Resource("multipoint.shp")
  val pointZ = Resource("pointz.shp")
  val polygonZ = Resource("w001n05f.shp")
  val polyLineZ = Resource("polylinez.shp")
  val polyLineZM = Resource("polylinezm.shp")
  val pointZM = Resource("pointzm.shp")
  val pointM = Resource("pointm.shp")
  val polyLineM = Resource("polylinem.shp")
  val polygonM = Resource("polygonm.shp")

  val polygonMContents = Vector(
    Feature(
      1,
      PolygonM(
        BBox(-118.1331320008404, 33.85400749666561, -118.13211202890042, 33.85515572855014),
        Range(5.0, 8.0),
        Vector(
          Vector(
            PointM(-118.1331320008404, 33.85497860767433, 5.0),
            PointM(-118.1327838667052, 33.85515572855014, 8.0),
            PointM(-118.13211202890042, 33.85453885929302, 7.0),
            PointM(-118.13306481705992, 33.85400749666561, 6.0),
            PointM(-118.1331320008404, 33.85497860767433, 5.0)
          )
        )
      ),
      Map("id" -> DBFNumeric(1), "test" -> DBFString("asdf"))
    ),
    Feature(
      2,
      PolygonM(
        BBox(-118.13351678067404, 33.8557726533132, -118.13267993607637, 33.85629785281825),
        Range(1.0, 4.0),
        Vector(
          Vector(
            PointM(-118.13351678067404, 33.85629785281825, 1.0),
            PointM(-118.13267993607637, 33.85599995063705, 4.0),
            PointM(-118.1329210089956, 33.8557726533132, 3.0),
            PointM(-118.13324804555214, 33.8559069653682, 2.0),
            PointM(-118.13351678067404, 33.85629785281825, 1.0)
          )
        )
      ),
      Map("id" -> DBFNumeric(2), "test" -> DBFString("qwer"))
    )
  )
}
