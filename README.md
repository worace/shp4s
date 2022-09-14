# shp4s - Pure Scala Shapefile Codec

Based on scodec and fs2.

## Quick-Start

### Installation

Published for Scala 2.12 and 2.13

```scala
libraryDependencies += "works.worace" %% "shp4s-core" % "0.2.1"
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

val featureStream: fs2.Stream[IO, Feature] = Core.streamShapefile("./path/to/my.shp")
```

### Converting Shapefile Features to JTS Geometries

```scala
import org.locationtech.jts.geom.Geometry
import works.worace.shp4s.Core
import works.worace.shp4s.Shape
import works.worace.shp4s.Feature
import works.worace.shp4s.DBFValue
import works.worace.shp4s.Jts
import works.worace.shp4s.JtsFeature

val features: Vector[Feature] = Core.readAllSync("./path/to/file.shp")

val feature: Feature = features.head

// Convert just the feature's geometry
val geom: Geometry = Jts.shapeToJts(feature.shape)

// Convert feature and pass through properties
val jtsFeat: JtsFeature = Jts.featureToJts(feature)
// JtsFeature(
//   geometry=Point (-118.13306035523306 33.85531587901563),
//   properties=Map(id -> DBFNumeric(1))
// )
```

Or you can use the provided implicit classes

```scala
import org.locationtech.jts.geom.Geometry
import works.worace.shp4s.Core
import works.worace.shp4s.Shape
import works.worace.shp4s.Feature
import works.worace.shp4s.DBFValue
import works.worace.shp4s.JtsFeature
import works.worace.shp4s.Jts.implicits._

val features: Vector[Feature] = Core.readAllSync("./path/to/file.shp")
val feature: Feature = features.head
feature.toJts() // JtsFeature(...)
```

## Development

* Run tests with `sbt test`
* Push Scaladoc to GitHub Pages: `sbt ghpagesPushSite`
* Run scalafmt: `sbt scalafmtAll`

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
