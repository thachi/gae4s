import net.virtualvoid.sbt.graph.Plugin._
import sbt.Keys._
import sbt._
import sbtappengine.Plugin._
import sbtbuildinfo.Plugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.ReleaseStep

object Gae4sBuild extends Build {

  object Versions {
    val scala = "2.11.7"
    val appengine = "1.9.30"
    val gcs = "0.5"
    val json4s = "3.3.0"
    val scalatest = "2.2.5"
  }

  lazy val root = Project(
    id = "gae4s-project",
    base = file("."),
    aggregate = Seq(core, scalatest),
    settings = defaultSetting ++ doNotPublish
  )

  lazy val core = Project(
    id = "gae4s-core",
    base = file("core"),
    settings = defaultSetting ++ buildInfoSettings ++ graphSettings ++ Seq(
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
        "com.google.appengine" % "appengine-api-stubs" % Versions.appengine,
        "com.google.appengine" % "appengine-testing" % Versions.appengine,
        "org.scalatest" %% "scalatest" % Versions.scalatest
      )
    )
  )


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

  lazy val defaultSetting = Defaults.coreDefaultSettings ++ releaseSettings ++
    Seq(
      scalaVersion := Versions.scala,
      scalacOptions ++= Seq("-feature", "-deprecation"),
      organization := "com.xhachi",
      parallelExecution in Test := false,
      publishTo <<= version { (v: String) =>
        val base = "/Users/takashi/Documents/xhachi/repository"
        if (v.trim.endsWith("SNAPSHOT"))
          Some(Resolver.file("snapshot", file(base + "/maven2-snapshot")))
        else
          Some(Resolver.file("release", file(base + "/maven2")))
      },
      releaseProcess := Seq[ReleaseStep](
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
    )

  val defaultDependency = Seq(
    "org.json4s" %% "json4s-native" % Versions.json4s,
    "org.json4s" %% "json4s-ext" % Versions.json4s,
    "com.google.appengine" % "appengine-api-1.0-sdk" % Versions.appengine,
    "com.google.appengine.tools" % "appengine-gcs-client" % Versions.gcs,
    "com.google.appengine" % "appengine-tools-sdk" % Versions.appengine % "test",
    "com.google.appengine" % "appengine-api-stubs" % Versions.appengine % "test",
    "com.google.appengine" % "appengine-testing" % Versions.appengine % "test",
    "org.scalatest" %% "scalatest" % Versions.scalatest % "test"
  )
}
