ThisBuild / scalaVersion := "2.13.1"

val commonSettings = Seq(
  version := "0.0.3-SNAPSHOT",
  organization := "works.worace",
  homepage := Some(url("https://github.com/worace/shp4s")),
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "0.7.1" % Test
  ),
  crossScalaVersions := Seq("2.12.11", "2.13.1"),
  scalaVersion := "2.13.1",
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-deprecation", "-Ywarn-unused"),
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  developers := List(
    Developer(
      "worace",
      "Horace Williams",
      "horace@worace.works",
      url("https://worace.works")
    )
  ),
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true
)

lazy val root = Project(
  id = "root",
  base = file(".")
).settings(commonSettings: _*)
  .dependsOn(core)
  .aggregate(core)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(SiteScaladocPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    name := "shp4s",
    ghpagesCleanSite / excludeFilter := "CNAME",
    ScalaUnidoc / siteSubdirName  := "api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, ScalaUnidoc / siteSubdirName),
    git.remoteRepo := "git@github.com:worace/shp4s.git",
    ghpagesNoJekyll := true,
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials"),
    usePgpKeyHex("37169035A8BEDF1EC943E79308109B5E42E0C41D"),
  )

lazy val core = project
  .settings(commonSettings: _*)
  .settings(
    name := "shp4s-core",
    description := "Scodec based pure Scala Shapefile decoder"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-core" % "1.11.10",
      "co.fs2" %% "fs2-scodec" % "3.2.14",
      "co.fs2" %% "fs2-io" % "3.2.14",
      "com.github.albfernandez" % "javadbf" % "1.13.2",
      "org.locationtech.jts" % "jts-core" % "1.19.0"
    )
  )

Global / onChangedBuildSource := ReloadOnSourceChanges

addCommandAlias("lint", ";scalafixAll;scalafmtAll")
