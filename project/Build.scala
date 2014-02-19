import sbt._

object Gae4sBuild extends Build {

  import sbt.Keys._
  import sbtbuildinfo.Plugin._

  lazy val defaultSetting = Defaults.defaultSettings ++ buildInfoSettings ++
    Seq(
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
      scalaVersion := "2.10.3",
      organization := "com.xhachi.appbase",
      version := "0.2-SNAPSNOT",
      scalaVersion := "2.10.3",
      parallelExecution in Test := false
    )

  lazy val root = Project(
    id = "gae4s",
    base = file("."),
    settings = defaultSetting ++ Seq(
      name := "gae4s",
      buildInfoPackage := "com.xhachi.gae4s",
      libraryDependencies ++= defaultDependency
    )
  )

  val defaultDependency = {
    val appengineVersion = "1.8.9"
    Seq(
      "com.google.appengine" % "appengine-api-1.0-sdk" % appengineVersion,
      "com.google.appengine" % "appengine-api-stubs" % appengineVersion % "test",
      "com.google.appengine" % "appengine-testing" % appengineVersion % "test",
      "org.slf4j" % "slf4j-jdk14" % "1.7.5" % "test",
      "org.scalatest" %% "scalatest" % "2.0" % "test"
    )
  }
}
