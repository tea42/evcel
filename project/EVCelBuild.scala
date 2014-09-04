import sbt._
import Keys._

import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform._
import scoverage.ScoverageSbtPlugin._

object EVCelBuild extends Build {

  val buildOrganisation = "com.evcel"
  val buildVersion = "0.1"
  val buildScalaVersion = "2.10.4"

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganisation,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
  ) ++ SbtScalariform.buildSettings

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
  ).dependsOn(maths, core, daterange, quantity)

  def module(name: String) = {
    Project(
      id = name,
      base = file(name),
      settings = buildSettings
    ).settings(instrumentSettings: _*).settings(scalariformSettings: _*)
  }
}
