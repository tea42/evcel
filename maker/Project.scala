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
val calendar = module("calendar", daterange)
val curve = module_("curve", List(quantity, daterange, calendar, utils), List(calendar))
val eventstore = module("eventstore", curve, utils)
val marketdatastore = module_("marketdatastore", List(eventstore), List(eventstore))
val calendarstore = module_("calendarstore", List(calendar, eventstore), List(eventstore))
val instrument = module_("instrument", List(curve), List(curve, quantity))
val reports = module_("reports", List(instrument), List(curve, instrument))
val xl = module("xl", reports)
val server = module("server", calendarstore, marketdatastore, xl)

val evcel = new Project(
  name = "evcel",
  root = new File("."),
  immediateUpstreamModules = List(server),
  props = makerProps
)
import evcel._