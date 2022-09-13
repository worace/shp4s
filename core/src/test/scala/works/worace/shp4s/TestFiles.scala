package works.worace.shp4s

import scodec.bits.BitVector
import java.nio.file.{Files, Paths}
import java.net.URL
import java.nio.file.Path

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
  val polygonZM = Resource("polygonzm.shp")
}
