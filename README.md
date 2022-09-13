# shp4s - Pure Scala Shapefile Codec

Based on scodec and fs2.

## Quick-Start

### Installation

```scala
libraryDependencies += "works.worace" % "shp4s-core" % "@VERSION@"
```

### Basic Decoding

```scala
import works.worace.shp4s.Core
import works.worace.shp4s.Shape
import works.worace.shp4s.Feature
import works.worace.shp4s.DBFValue

val features: Vector[Feature] = Core.readAllSync("./path/to/file.shp")

val feature: Feature = features.head

// Shape geometry -- Point, PolyLine, etc
val shape: Shape = feature.shape

// Properties, from DBF file
val props: Map[String, DBFValue] = feature.properties
```

### Streaming Usage (with fs2)

If you don't want to read the whole file into memory, you can decode it in streaming fashion using [fs2](https://fs2.io/)

Note that you will need to use some [cats-effect](https://typelevel.org/cats-effect/) based tooling for this (fs2 streams resolve to an `IO` instance which needs to be evaluated).

```scala
import works.worace.shp4s.Core
import works.worace.shp4s.Shape
import works.worace.shp4s.Feature
import cats.effect.IO

val features: Stream[IO, Feature] = Core.streamShapefile("./path/to/my.shp")
```


```scala
import works.worace.shp4s
import cats.effect.unsafe.implicits.global

// Read features into Vector
shp4s.Core.readAllSync("test.shp")

// fs2.Stream[IO, Feature]
streamShapefile(path: String)
```

### Notes / TODO

Types - 14
* [X] NullShape
* [X] Point
* [X] PolyLine
* [X] Polygon
* [X] MultiPoint
* [X] PointZ
* [x] PolylineZ
* [x] PolygonZ
* [X] MultiPointZ
* [x] PointM
* [x] PolyLineM
* [x] PolygonM
* [x] MultiPointM
* [ ] MultiPatch
* [X] DBF
* [ ] DBF proper resource handling (cats bracket? closing in finally?)

TODO Edge Cases

* [x] Verify PointZ file with M values
* [ ] Verify MultiPointZ with M values
* [x] Convert MultiPointZ to hold Vector[PointZ] values
* [x] PolylineZ with M Values
* [ ] *-Z encoding with empty M values -- should omit entirely rather than encoding 0's
* [x] PolyLineZ Sample File
* [ ] Import test cases from gdal https://github.com/OSGeo/gdal/tree/master/autotest/ogr/data/shp

## Releasing

* Set version in `build.sbt` to next non-SNAPSHOT
* Make sure GPG and sonatype credentials are available -- `~/.sbt/sonatype_credentials`, `$PGP_PASSPHRASE`, and appropriate key as specified in `build.sbt`
* Run `sbt +publishSigned`
* Open sonatype staging repository: https://oss.sonatype.org/#stagingRepositories and complete the release
  * Check contents (should have a 2.13 and 2.12 version for each module)
  * "close" the release, then "release" using sonatype UI options
* Run `git tag vX.Y.Z` and push the tag
* Set version to `-SNAPSHOT` for _next_ version, commit, and push
* Note: I think [sbt-ci-release](https://github.com/sbt/sbt-ci-release) is better than doing this manually and i have used it previously on other projects but could not get it working this time for some reason `:rip:`
