import xerial.sbt.Sonatype._


ThisBuild / scalaVersion := "2.13.1"

val commonSettings = Seq(
  organization := "works.worace",
  homepage := Some(url("https://github.com/worace/shp4s")),
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "0.7.1" % Test
  ),
  crossScalaVersions := Seq("2.12.11", "2.13.1"),
  scalaVersion := "2.13.1",
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-deprecation"),
  licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  sonatypeProjectHosting := Some(GitHubHosting("worace", "shp4s", "horace@worace.works")),
  developers := List(
    Developer(
      "worace",
      "Horace Williams",
      "horace@worace.works",
      url("https://worace.works")
    )
  )
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
    excludeFilter in ghpagesCleanSite := "CNAME",
    siteSubdirName in ScalaUnidoc := "api",
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
    git.remoteRepo := "git@github.com:worace/shp4s.git",
    ghpagesNoJekyll := true
  )

lazy val core = project
  .settings(commonSettings: _*)
  .settings(
    name := "shp4s-core",
    description := "Scodec based pure Scala Shapefile decoder"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-core" % "1.11.7",
      "org.scodec" %% "scodec-stream" % "2.0.0",
      "co.fs2" %% "fs2-io" % "2.3.0"
    )
  )

lazy val docs = project
  .dependsOn(core)
  .in(file("usage"))
  .enablePlugins(MdocPlugin)
  .settings(
    mdocVariables := Map(
      "VERSION" -> version.value
    )
  )
