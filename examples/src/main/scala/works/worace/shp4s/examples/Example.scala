package works.worace.shp4s.examples

import blobstore.Store
import blobstore.gcs.GcsStore
import com.google.cloud.storage.{Acl, Blob, BlobId, BlobInfo, Storage}

import cats.effect._
import com.google.cloud.storage.StorageOptions
import java.nio.file.Path
import java.nio.channels.Channel
import java.nio.channels.Channels

import works.worace.shp4s._
import Core.Props

object Example extends IOApp {
  import cats.effect.{ExitCode, IO, IOApp, Resource}
  import cats.implicits._

  def run(args: List[String]): IO[ExitCode] = {
    val path = args.head
    println(path)

    println("build storage")
    val storage = StorageOptions.newBuilder().build().getService()
    println(storage)
    val fs2Store = new GcsStore(storage, List(), false, false)

    val runStream: IO[ExitCode] = Core
      .streamShapefile(path)
      .map(_.toString)
      .intersperse("\n")
      .take(0)
      .map { l =>
        println(l)
        l
      }
      .compile
      .drain
      .as(ExitCode.Success)

      val shapes = readGcsShps("/shp_test/cb_2018_us_county_500k.shp", fs2Store)
      val props = readGcsProps("shp_test", "cb_2018_us_county_500k.dbf", storage)

      println(shapes)
      println(props)

      Core.featureStream(shapes, props)
        .take(1)
        .map(_.toString)
        .through(debug)
        .compile
        .drain
        .as(ExitCode.Success)
  }

  def debug(s: fs2.Stream[IO, String]): fs2.Stream[IO, String] = {
    s.evalMap { e =>
      IO {
        println(e)
        e
      }
    }
  }

  def readGcsProps(bucket: String, file: String, storage: Storage): fs2.Stream[IO, Props] = {
    val b: Blob = storage.get(bucket, file)
    val is = Channels.newInputStream(b.reader())
    DBFIterator(is).stream
  }

  def readGcsShps(gcsPath: String, store: GcsStore[IO]): fs2.Stream[IO, ShapeRecord] = {
    Core.streamShapeRecords(store.get(Paths.get(gcsPath), 4096))
  }
}
