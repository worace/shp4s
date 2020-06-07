package works.worace.shp4s

import scala.io.Source
import java.nio.file.{Files, Paths}

class CoreTest extends munit.FunSuite {
  val shp = getClass.getResource("/world-cities.shp")
  val shpBytes = Files.readAllBytes(Paths.get(shp.getPath))
  test("...") {
    assertEquals("hi", Core.hi)
    println(shp.getPath)

    println(shpBytes)
    Core.readHeader(shpBytes)
  }

  test("streaming points") {
    
  }
}
