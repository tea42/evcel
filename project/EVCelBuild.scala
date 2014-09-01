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


  lazy val coreDependencies = Seq(
    "colt" % "colt" % "1.2.0",
    "org.scalanlp" %% "breeze-math" % "0.4",
    "org.apache.commons" % "commons-math3" % "3.3",
    "commons-io" % "commons-io" % "2.4",
    "org.scalatest" %% "scalatest" % "2.0" % "test"
  )


  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildSettings ++
      Seq(libraryDependencies ++= coreDependencies)
  ).settings(instrumentSettings: _*).settings(scalariformSettings:_*)
}
