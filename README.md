# shp4s - Pure Scala Shapefile Codec

Based on scodec and fs2.

```scala
import works.worace.shp4s
import cats.effect.unsafe.implicits.global

// Read features into Vector
shp4s.Core.readAllSync("test.shp")

// fs2.Stream[IO, Feature]
streamShapefile(path: String)
```

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
* [ ] Convert MultiPointZ to hold Vector[PointZ] values
* [x] PolylineZ with M Values
* [ ] *-Z encoding with empty M values -- should omit entirely rather than encoding 0's
* [x] PolyLineZ Sample File

## Releasing

* Set version in `build.sbt` to next non-SNAPSHOT
* Make sure GPG and sonatype credentials are available -- `~/.sbt/sonatype_credentials`, `$PGP_PASSPHRASE`, and appropriate key as specified in `build.sbt`
* Run `sbt +publishSigned`
* Open sonatype staging repository: https://oss.sonatype.org/#stagingRepositories and complete the release
  * Check contents (should have a 2.13 and 2.12 version for each module)
  * "close" the release, then "release" using sonatype UI options
* Run `git tag vX.Y.Z` and push the tag
* Set version to `-SNAPSHOT` for _next_ version, commit, and push
