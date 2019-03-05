
import ReleaseTransformations._

val appengineVersion = "1.9.71"
val gcsVersion = "0.5"
val json4sVersion = "3.3.0"
val scalatestVersion = "3.0.5"

ThisBuild / organization := "com.xhachi"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / crossScalaVersions := "2.11.12" :: "2.12.8" :: Nil
ThisBuild / parallelExecution in Test := false

ThisBuild / releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  // tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

lazy val root = (project in file("."))
  .settings(Seq(publish := {}, publishLocal := {}))
  .settings(
    name := "gae4s-project"
  )
  .aggregate(core, scalatest)

lazy val core = (project in file("gae4s-core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "gae4s-core",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.xhachi.gae4s.buildinfo",
    libraryDependencies ++= defaultDependency ++ Seq(
    )
  ).dependsOn(scalatest % "test")

lazy val scalatest = (project in file("gae4s-scalatest"))
  .settings(
    name := "gae4s-scalatest",
    libraryDependencies ++= defaultDependency ++ Seq(
      "com.google.appengine" % "appengine-api-stubs" % appengineVersion,
      "com.google.appengine" % "appengine-testing" % appengineVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion
    )
  )

lazy val sample = (project in file("gae4s-sample"))
  .enablePlugins(AppenginePlugin)
  .settings(Seq(publish := {}, publishLocal := {}))
  .settings(
    name := "gae4s-sample",
    libraryDependencies ++= defaultDependency ++ Seq(
      "javax.servlet" % "servlet-api" % "2.5" % "provided",
      "org.eclipse.jetty" % "jetty-webapp" % "7.0.2.v20100331" % "container"
    )
  )
  .dependsOn(core, scalatest % "test")

val defaultDependency = Seq(
  "com.google.appengine" % "appengine-api-1.0-sdk" % appengineVersion,
  "com.google.appengine.tools" % "appengine-gcs-client" % gcsVersion,
  "com.google.appengine" % "appengine-tools-sdk" % appengineVersion % "test",
  "com.google.appengine" % "appengine-api-stubs" % appengineVersion % "test",
  "com.google.appengine" % "appengine-testing" % appengineVersion % "test",
  "org.scalatest" %% "scalatest" % scalatestVersion % "test"
)
