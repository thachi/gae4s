organization := "com.xhachi.appbase"

name := "gae4s"

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= {
  val appengineVersion = "1.8.9"
  Seq(
    "com.google.appengine" % "appengine-api-1.0-sdk" % appengineVersion,
    "com.google.appengine" % "appengine-api-stubs" % appengineVersion % "test",
    "com.google.appengine" % "appengine-testing" % appengineVersion % "test",
    "org.slf4j" % "slf4j-jdk14" % "1.7.5" % "test",
    "org.scalatest" %% "scalatest" % "2.0" % "test"
  )
}

parallelExecution in Test := false
