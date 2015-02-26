import sbt.Keys._
import sbt._
import sbtappengine.Plugin._
import sbtrelease.ReleasePlugin._

object Gae4sBuild extends Build {

  import Settings._
  import Versions._
  import sbt.Keys._
  import sbtbuildinfo.Plugin._
  import sbtrelease._
  import ReleaseStateTransformations._

  ReleaseKeys.releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
//    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    pushChanges
  )


  lazy val root = Project(
    id = "gae4s-project",
    base = file("."),
    aggregate = Seq(core, scalatest, store_ext),
    settings = defaultSetting ++ doNotPublish
  )

  lazy val core = Project(
    id = "gae4s-core",
    base = file("core"),
    settings = defaultSetting ++ buildInfoSettings ++ Seq(
      name := "gae4s-core",
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
      buildInfoPackage := "com.xhachi.gae4s.buildinfo",
      libraryDependencies ++= defaultDependency ++ Seq(

      )
    )
  ).dependsOn(scalatest % "test")

  lazy val scalatest = Project(
    id = "gae4s-scalatest",
    base = file("scalatest"),
    settings = defaultSetting ++ Seq(
      name := "gae4s-scalatest",
      libraryDependencies ++= defaultDependency ++ Seq(
        "com.google.appengine" % "appengine-api-stubs" % appengineVersion,
        "com.google.appengine" % "appengine-testing" % appengineVersion,
        "org.scalatest" %% "scalatest" % scalatestVersion
      )
    )
  )

  lazy val store_ext = Project(
    id = "gae4s-store-extension",
    base = file("store-extension"),
    settings = defaultSetting ++ Seq(
      name := "gae4s-store-extension",
      libraryDependencies ++= defaultDependency ++ Seq(

      )
    )
  ).dependsOn(core, scalatest % "test")

  lazy val sample = Project(
    id = "gae4s-sample",
    base = file("sample"),
    settings = defaultSetting ++ doNotPublish ++ appengineSettings ++ Seq(
      name := "gae4s-sample",
      libraryDependencies ++= defaultDependency ++ Seq(
        "javax.servlet" % "servlet-api" % "2.5" % "provided",
        "org.eclipse.jetty" % "jetty-webapp" % "7.0.2.v20100331" % "container"
      ),
      packagedArtifacts := Map.empty
    )
  ).dependsOn(core, scalatest % "test")

  lazy val doNotPublish = Seq(publish := {}, publishLocal := {})

}

object Settings {

  import Versions._

  lazy val defaultSetting = Defaults.defaultSettings ++ releaseSettings ++
    Seq(
      scalaVersion := "2.11.5",
      scalacOptions ++= Seq("-feature", "-deprecation"),
      organization := "com.xhachi",
      parallelExecution in Test := false,
      publishTo <<= version { (v: String) =>
        val base = "/Users/takashi/Documents/xhachi/repository"
        if (v.trim.endsWith("SNAPSHOT"))
          Some(Resolver.file("snapshot", file(base + "/maven2-snapshot")))
        else
          Some(Resolver.file("release", file(base + "/maven2")))
      }
    )

  val defaultDependency = Seq(
    "org.json4s" %% "json4s-native" % "3.2.11",
    "org.json4s" %% "json4s-ext" % "3.2.11",
    "com.google.appengine" % "appengine-api-1.0-sdk" % appengineVersion,
    "com.google.appengine.tools" % "appengine-gcs-client" % "0.4.4",
    "com.google.appengine" % "appengine-api-stubs" % appengineVersion % "test",
    "com.google.appengine" % "appengine-testing" % appengineVersion % "test",
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )
}

object Versions {
  val appengineVersion = "1.9.18"
  val scalatestVersion = "2.2.4"
}