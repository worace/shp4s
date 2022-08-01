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

* no `version` setting in build.sbt
* push a git tag (e.g. `v0.0.1`) to release a version
* other commits to master will be pushed as snapshots
