import maker.project.{Module, Project, ClassicLayout}
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
def module_(name : String, upstream : List[Module], testUpstream : List[Module]) : Module = {
  new Module(
    root = new File(name), 
    projectRoot_ = new File("."),
    name = name, 
    immediateUpstreamModules = upstream,
    immediateUpstreamTestModules = testUpstream
    ) with ClassicLayout{
      override def scalacOptions = List("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
    }
}
def module(name : String, upstream : Module*) : Module = {
  module_(name, upstream.toList, testUpstream = Nil)
}

val utils = module("utils")
val daterange = module("daterange", utils)
val maths = module("maths", daterange, utils)
val quantity = module("quantity", maths, utils)
val pivot = module("pivot", quantity, daterange)
val referencedata = module("referencedata", daterange, quantity)
val curve = module_("curve", List(referencedata, quantity, maths, utils), List(referencedata, quantity, utils))
val instrument = module_("instrument", List(curve), List(curve, quantity))
val valuation = module_("valuation", List(instrument, curve), List(curve, quantity))

val eventstore = module("eventstore", instrument, curve, utils)
val marketdatastore = module_("marketdatastore", List(eventstore), List(eventstore, curve))
val referencedatastore = module_("referencedatastore", List(referencedata, eventstore), List(eventstore))
val tradestore = module_("tradestore", List(instrument, eventstore), List(eventstore))
val reports = module_("reports", List(pivot, valuation), List(curve, instrument, valuation))
val xl = module("xl", reports)
val server = module_("server", List(referencedatastore, marketdatastore, tradestore, xl), List(eventstore, marketdatastore))

val evcel = new Project(
  name = "evcel",
  root = new File("."),
  immediateUpstreamModules = List(server)
)
import evcel._
