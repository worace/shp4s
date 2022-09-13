# shp4s

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
