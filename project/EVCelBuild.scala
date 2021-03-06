import sbt._
import Keys._

import scala.util.Properties

object EVCelBuild extends Build {

  Properties.setProp("logback.configurationFile", "config/logback-unit-tests.xml")
  val buildOrganisation = "com.evcel"
  val buildVersion = "0.1"
  val buildScalaVersion = "2.11.5"
  val buildScalaMajorVersion = "2.11"

  lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    updateOptions := updateOptions.value.withCachedResolution(cachedResoluton = true),
    organization := buildOrganisation,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
  )

  lazy val utils = module("utils").settings(
    libraryDependencies ++= Seq(
      "com.github.stacycurl" %% "pimpathon-core" % "1.1.0",
      "org.scalaz" %% "scalaz-core" % "7.1.1",
      "com.chuusai" %% "shapeless" % "2.0.0"
    )
  )

  lazy val maths = module("maths").settings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.3",
      "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      "com.opengamma" % "og-analytics" % "2.0.0-alpha-12",
      "com.opengamma" % "og-util" % "2.0.0-alpha-12",
      "org.scalanlp" %% "breeze" % "0.10",
      "org.scalanlp" %% "breeze-natives" % "0.10"
    ),
    resolvers += "opengamma" at "http://maven.opengamma.com/nexus/content/groups/public/"
  ).dependsOn(utils, daterange)

  lazy val daterange = module("daterange").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(utils)

  lazy val quantity = module("quantity").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(maths, utils)

  lazy val pivot = module("pivot").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(quantity, daterange)

  lazy val instrument = module("instrument").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(curve % "compile->compile;test->test", quantity % "test->test")

  lazy val valuation = module("valuation").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(instrument, curve % "compile->compile;test->test", quantity % "test->test")
  
  lazy val reports = module("reports").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(pivot, valuation % "compile->compile;test->test", instrument % "compile->compile;test->test", curve % "test->test", quantity % "test->test")

  lazy val xl = module("xl").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(reports)

  lazy val referencedata = module("referencedata").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(daterange, quantity, utils % "test->test")

  lazy val curve = module("curve").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    ),
    resourceDirectory in Test := baseDirectory.value / "test-resources"
  ).dependsOn(calendar, utils, quantity %  "compile->compile;test->test",
      daterange, referencedata % "compile->compile;test->test", utils % "test->test")

  lazy val eventstore = module("eventstore").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      "org.apache.kafka" % s"kafka_$buildScalaMajorVersion" % "0.8.2.0" exclude ("org.slf4j", "slf4j-log4j12"),
      "org.apache.zookeeper" % "zookeeper" % "3.3.4" exclude ("org.slf4j", "slf4j-log4j12"),
      "io.spray" % s"spray-json_$buildScalaMajorVersion" % "1.2.6"
    ).map(
      _.exclude ("com.sun.jdmk", "jmxtools")
    ).map(
      _.exclude("javax.jms", "jms")
    ).map(
      _.exclude("com.sun.jmx", "jmxri")
    )
  ).dependsOn(instrument, referencedata, curve, quantity, utils)

  lazy val calendar = module("calendar").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(daterange)


  lazy val marketdatastore = module("marketdatastore").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(eventstore % "compile->compile;test->test", curve % "compile->compile;test->test")

  lazy val referencedatastore = module("referencedatastore").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(referencedata, eventstore % "compile->compile;test->test")

  lazy val tradestore = module("tradestore").settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )
  ).dependsOn(instrument, eventstore % "compile->compile;test->test")

  lazy val server = module("server").settings(
    libraryDependencies ++= Seq(),
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests"
  ).dependsOn(maths, daterange, quantity, eventstore, calendar, tradestore, referencedatastore, xl, instrument, marketdatastore)

  def module(name: String) = {
    Project(
      id = name,
      base = file(name),
      settings = buildSettings
        ++ addCommandAlias("gen-idea-with-sources", ";update-classifiers;update-sbt-classifiers;gen-idea sbt-classifiers")
    ).settings(
        scalaSource in Compile := baseDirectory.value / "src",
        scalaSource in Test := baseDirectory.value / "tests"
      )
  }
}
