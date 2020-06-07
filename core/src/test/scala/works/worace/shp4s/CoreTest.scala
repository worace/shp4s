package works.worace.shp4s

import scala.io.Source
import java.nio.file.{Files, Paths}
import cats.effect.{IO, ContextShift}

class CoreTest extends munit.FunSuite {
  implicit val csIO: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
  val shp = getClass.getResource("/world-cities.shp")
  val shpBytes = Files.readAllBytes(Paths.get(shp.getPath))
  test("...") {
    assertEquals("hi", Core.hi)
    println(shp.getPath)

    println(shpBytes)
    Core.readHeader(shpBytes)
  }

  test("streaming points") {
    val stream = Core
      .streamShapefile(Paths.get(shp.getPath))
      .map { rec =>
        println("stream rec...")
        println(rec)
        rec
      }
    println(stream)
    println(stream.compile)
    println(stream.compile.toVector.unsafeRunSync())
  }
}
