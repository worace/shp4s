ThisBuild / scalaVersion := "2.13.1"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "0.7.1" % Test
  ),
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= Seq("-feature", "-deprecation", "-Ywarn-unused"),
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq("2.12.11", "2.13.1"),
  // Publish-related settings
  organization := "works.worace",
  homepage := Some(url("https://github.com/worace/shp4s")),
  licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  developers := List(
    Developer(
      "worace",
      "Horace Williams",
      "horace@worace.works",
      url("https://worace.works")
    )
  ),
  ThisBuild / pomIncludeRepository := { _ => false },
  ThisBuild / publishMavenStyle := true,
  publishTo := sonatypePublishTo.value,
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
    publishArtifact := false,
  )

lazy val core = project
  .settings(commonSettings: _*)
  .settings(
    name := "shp4s-core",
    description := "Scodec based pure Scala Shapefile decoder"
  )
  .settings(
    publishArtifact := true,
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
addCommandAlias("ci", "+test;scalafmtCheck;scalafixAll --check")
// SBT dynver/sonatype release
// To publish a git-tagged snapshot, just run:
addCommandAlias("release", "+publishSigned;sonatypeRelease")
// To publish a 'real' version, tag the current commit with a tag matching vX.Y.Z
// e.g. git tag v0.2.0
// then run sbt release
