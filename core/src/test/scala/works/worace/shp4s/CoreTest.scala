package works.worace.shp4s

import scala.io.Source
import java.nio.file.{Files, Paths}
import cats.effect.{IO, ContextShift}
import java.net.URL
import java.nio.file.Path
import scodec.bits.BitVector

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
  val multiPoints = Resource("multipoint.shp")
}

class CoreTest extends munit.FunSuite {
  implicit val csIO: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  test("...") {
    println(TestFiles.points.path)

    Core.readHeader(TestFiles.points.bytes)
  }

  test("streaming points") {
    val points = Core.readAllSync(TestFiles.points.path)
    assertEquals(points.size, 2539)
    val shapeTypes = points.map(_.shape).map(_.getClass.getName()).toSet
    assertEquals(shapeTypes, Set("works.worace.shp4s.Core$Point"))
  }

  test("multipoints") {
    import scodec.bits._
    import scodec._
    import scodec.codecs._
    println("MPS*********")
    val bv = TestFiles.multiPoints.bitvec
    val h = Core.header.decode(bv).getOrElse(throw new RuntimeException)
    println(h)
    val rech = Core.recordHeader.decode(h.remainder).getOrElse(throw new RuntimeException)
    println(rech)


    println("******")
    for {
      shptype <- int32L.decode(rech.remainder)
      body <- Core.multiPointZ.decode(shptype.remainder)
      // bbox <- Core.bbox.decode(shptype.remainder)
      // points <- vectorOfN(int32L, Core.point).decode(bbox.remainder)
      // zrange <- Core.zRange.decode(points.remainder)
      // zvals <- doubleL.decode(zrange.remainder)
      // mrange <- Core.mRange.decode(zvals.remainder)
      // numPoints <- int32L.decode(bbox.remainder)
      // point <- Core.point.decode(numPoints.remainder)
    } yield {
      println(body)
      // println(s"shptyp: $shptype")
      // println(s"bbox: $bbox")
      // println(s"points: $points")
      // println(s"zrange: $zrange")
      // println(s"zvals: $zvals")

      // NOT THERE
      // println(s"mrange: $mrange")
      // println(s"numPoints: $numPoints")
      // println(s"point: $point")
    }


    // val shapetype = int32L.decode(rech.remainder)
    // println(shapetype)
    // val mpz = Core.multiPointZ.decode(rech.remainder)
    // println(mpz)

    // val mps = Core.readAllSync(TestFiles.multiPoints.path)
    // println(mps)
    // Core.readHeader(TestFiles.multiPoints.bytes)
  }
}
