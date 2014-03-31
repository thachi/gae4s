import sbt._

object Gae4sBuild extends Build {

  import sbt.Keys._
  import sbtbuildinfo.Plugin._

  lazy val defaultSetting = Defaults.defaultSettings ++ buildInfoSettings ++
    Seq(
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
      scalaVersion := "2.10.3",
      organization := "com.xhachi",
      version := "0.5-SNAPSHOT",
      publishTo := Some(Resolver.file("gae4s",file("/Users/takashi/Documents/xhachi/maven/maven"))(Patterns(true, Resolver.mavenStyleBasePattern))),
      parallelExecution in Test := false
    )

  lazy val root = Project(
    id = "gae4s",
    base = file("."),
    settings = defaultSetting ++ Seq(
      name := "gae4s",
      buildInfoPackage := "com.xhachi.gae4s.buildinfo",
      libraryDependencies ++= defaultDependency
    )
  )


  val defaultDependency = {
    val appengineVersion = "1.9.1"
    Seq(
      "org.json4s" %% "json4s-native" % "3.2.7",
      "com.google.appengine" % "appengine-api-1.0-sdk" % appengineVersion,
      "com.google.appengine" % "appengine-api-stubs" % appengineVersion % "test",
      "com.google.appengine" % "appengine-testing" % appengineVersion % "test",
      "org.slf4j" % "slf4j-jdk14" % "1.7.5" % "test",
      "org.scalatest" %% "scalatest" % "2.0" % "test"
    )
  }
}
