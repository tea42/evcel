import sbt._
import Keys._

import scala.util.Properties
import org.scalatra.sbt._
import org.scalatra.sbt.PluginKeys._
import com.mojolly.scalate.ScalatePlugin._
import ScalateKeys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
//import com.earldouglas.xsbtwebplugin.PluginKeys.webappResources

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
      "com.opengamma" % "og-analytics" % "2.0.0-alpha-12" notTransitive(),
      "com.opengamma" % "og-util" % "2.0.0-alpha-12" notTransitive(),
      "commons-lang" % "commons-lang" % "2.6" notTransitive(),
      "com.google.guava" % "guava" % "12.0" notTransitive(),
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
  ).dependsOn(maths, daterange, quantity, eventstore, calendar, tradestore, referencedatastore, xl, instrument, marketdatastore, webjs)

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

  lazy val webshared = module("webshared").settings(
    scalaSource in Compile := baseDirectory.value / "src"
  ).enablePlugins(ScalaJSPlugin)

  lazy val webjs = module("webjs").settings(
      scalaSource in Compile := baseDirectory.value / "src",
      resolvers += "bintray/non" at "http://dl.bintray.com/non/maven", //for upickle
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.8.0",
        "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
        "com.lihaoyi" %%% "upickle" % "0.2.6"
      )
  ).dependsOn(webshared).enablePlugins(ScalaJSPlugin)

  lazy val webjvm = {
    val ScalatraVersion = "2.3.0"
    val JettyVersion = "9.1.3.v20140225"
    module("webjvm").settings(
        /* ScalatraPlugin.scalatraSettings ++
           the above line enables sbt web integration but requires removing jetty classes from the classpath
           which are needed for websocket support when running JettyLauncher
         */
        (scalateSettings ++ Seq(
        scalaSource in Compile := baseDirectory.value / "src",
        //webappResources in Compile := Seq(baseDirectory.value / "webapp"),
        resourceDirectory in Compile := baseDirectory.value / "resources",
        name := "A name",
        resolvers += Classpaths.typesafeReleases,
        resolvers += "bintray/non" at "http://dl.bintray.com/non/maven", //for upickle
        libraryDependencies ++= Seq(
          "org.scalatra" %% "scalatra" % ScalatraVersion,
          "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
          "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
          "ch.qos.logback" % "logback-core" % "1.1.2" % "runtime",
          "ch.qos.logback" % "logback-classic" % "1.1.2" % "runtime",
          "org.eclipse.jetty" % "jetty-webapp" % JettyVersion % "compile",
          "org.eclipse.jetty" % "jetty-plus" % JettyVersion % "compile;provided",
          "javax.servlet" % "javax.servlet-api" % "3.1.0" % "compile;provided;test",

          "org.scalatra" %% "scalatra-atmosphere" % ScalatraVersion ,
          "org.scalatra" %% "scalatra-json" % ScalatraVersion ,
          "org.json4s"   %% "json4s-jackson" % "3.2.9",
          "org.eclipse.jetty.websocket" % "websocket-server" % JettyVersion % "compile;provided",

          "com.lihaoyi" %%% "upickle" % "0.2.6",

          "commons-codec" % "commons-codec" % "1.6" notTransitive(),
          "commons-io" % "commons-io" % "2.0.1" notTransitive()
        ),
        scalateTemplateConfig in Compile <<= (sourceDirectory in Compile){ base =>
          Seq(
            TemplateConfig(
              base / "webapp" / "WEB-INF" / "templates",
              Seq.empty,  /* default imports should be added here */
              Seq(
                Binding("context", "_root_.org.scalatra.scalate.ScalatraRenderContext", importMembers = true, isImplicit = true)
              ),  /* add extra bindings here */
              Some("templates")
            )
          )
        }
      )) :_*
    ).dependsOn(server, webshared)
  }

}
