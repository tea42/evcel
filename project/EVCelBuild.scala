import sbt._
import Keys._

import scoverage.ScoverageSbtPlugin._

object EVCelBuild extends Build {

  val buildOrganisation = "com.evcel"
  val buildVersion = "0.1"
  val buildScalaVersion = "2.10.4"
  val buildScalaMajorVersion = "2.10"

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganisation,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
  ) 

  lazy val maths = module("maths").settings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.3",
      "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      "com.opengamma" % "og-analytics" % "2.0.0-alpha-12",
      "com.opengamma" % "og-util" % "2.0.0-alpha-12" 
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests",
    resolvers += "opengamma" at "http://maven.opengamma.com/nexus/content/groups/public/"
  )

  lazy val daterange = module("daterange").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  )

  lazy val quantity = module("quantity").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  ).dependsOn(maths)

  lazy val curve = module("curve").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  ).dependsOn(quantity, daterange)

  lazy val eventstore = module("eventstore").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      "org.apache.kafka" % s"kafka_$buildScalaMajorVersion" % "0.8.1.1" exclude ("org.slf4j", "slf4j-log4j12"),
      "org.apache.zookeeper" % "zookeeper" % "3.4.6" exclude ("org.slf4j", "slf4j-log4j12"),
      "io.spray" % s"spray-json_$buildScalaMajorVersion" % "1.2.6"
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  ).dependsOn(curve, quantity)

  lazy val calendar = module("calendar").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  ).dependsOn(daterange)

  lazy val calendarstore = module("calendarstore").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  ).dependsOn(calendar, eventstore)

  lazy val core = module("core").settings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.3",
      "colt" % "colt" % "1.2.0",
      "org.scalanlp" %% "breeze-math" % "0.4",
      "commons-io" % "commons-io" % "2.4",
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    ),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  ).dependsOn(maths)

  lazy val server = module("server").settings(
    libraryDependencies ++= Seq()
  ).dependsOn(maths, core, daterange, quantity, eventstore, calendar, calendarstore)

  def module(name: String) = {
    Project(
      id = name,
      base = file(name),
      settings = buildSettings
    ).settings(instrumentSettings: _*)
  }
}
