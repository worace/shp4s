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
  // Test files copied from GDAL test suite
  // https://github.com/OSGeo/gdal/tree/master/autotest/ogr/data/shp
  val gdalGjMultiPoly = Resource("gjmultipoly.shp")

  // https://gis-pdx.opendata.arcgis.com/datasets/PDX::zip-code-boundaries
  // City of portland zipcode boundaries -- multipolygons with holes
  // Original file:
  // val portlandZips = Resource("Zip_Code_Boundaries.shp")
  // Same file projected to 4326 via:
  // ogr2ogr portland_zips.shp -t_srs "EPSG:4326" Zip_Code_Boundaries.shp
  val portlandZips = Resource("portland_zips.shp")

  // US Census sample data -- seems to have a null value in one
  // of its numeric fields
  val census = Resource("tl_2020_60_tabblock20.shp")
  val censusDbf = Resource("tl_2020_60_tabblock20.dbf")
}
