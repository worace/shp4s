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
  val dbf = Resource("world-cities.dbf")
  val points = Resource("world-cities.shp")
  val multiPointZ = Resource("multipointZ.shp")
  val polyline = Resource("usa-major-highways.shp")
  val polygon = Resource("ne_10m_admin_0_antarctic_claims.shp")
  val multiPoint = Resource("multipoint.shp")
  val pointZ = Resource("pointz.shp")
}
