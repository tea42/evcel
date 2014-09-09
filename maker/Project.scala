import maker.project.{Module, Project}
import java.io.File
import scala.util.Properties
import maker.task.BuildResult
import maker.task.Dependency
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler
import maker.MakerProps

Properties.setProp("scala.usejavacp", "false")
Properties.setProp("log4j.ignoreTCL", "true")
Properties.clearProp("scala.home")
Properties.setProp("logback.configurationFile", "config/logback-unit-tests.xml")
val makerProps = MakerProps("LogbackTestConfigFile", "config/logback-unit-tests.xml")
def module_(name : String, upstream : List[Module], testUpstream : List[Module]) : Module = {
  new Module(
    root = new File(name), 
    name = name, 
    immediateUpstreamModules = upstream,
    immediateUpstreamTestModules = testUpstream,
    props = makerProps
    )
}
def module(name : String, upstream : Module*) : Module = {
  module_(name, upstream.toList, testUpstream = Nil)
}

val utils = module("utils")
val maths = module("maths")
val daterange = module("daterange", maths)
val quantity = module("quantity", maths)
val curve = module("curve", quantity, daterange)
val eventstore = module("eventstore", curve, utils)
val calendar = module("calendar", daterange)
val calendarstore = module_("calendarstore", List(calendar, eventstore), List(eventstore))
val evcel = new Project(
  name = "evcel",
  root = new File("."),
  immediateUpstreamModules = List(calendarstore),
  props = makerProps
)

import evcel._
