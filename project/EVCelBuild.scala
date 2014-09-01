import sbt._
import Keys._

object EVCelBuild extends Build {

	val buildOrganisation = "com.evcel"
	val buildVersion = "0.1"
	val buildScalaVersion = "2.10.4"

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
        organization  := buildOrganisation,
        version       := buildVersion,
        scalaVersion  := buildScalaVersion
                      )

	lazy val coreDependencies = Seq(
		"colt" % "colt" % "1.2.0",
		"org.scalanlp" %% "breeze-math" % "0.4",
		"org.apache.commons" % "commons-math3" % "3.3",
		"commons-io" % "commons-io" % "2.4",
    "org.scalatest" %% "scalatest" % "2.0" % "test"
	)

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

	lazy val core = Project(
		id = "core",
		base = file("core"),
		settings = buildSettings ++
			Seq(libraryDependencies ++= coreDependencies)
	)
}
