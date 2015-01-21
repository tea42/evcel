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
val makerProps = MakerProps("LogbackTestConfigFile", "config/logback-unit-tests.xml")
def module_(name : String, upstream : List[Module], testUpstream : List[Module]) : Module = {
  new Module(
    root = new File(name), 
    name = name, 
    immediateUpstreamModules = upstream,
    immediateUpstreamTestModules = testUpstream,
    props = makerProps
    ) with ClassicLayout{
      override def scalacOptions = List("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
    }
}
def module(name : String, upstream : Module*) : Module = {
  module_(name, upstream.toList, testUpstream = Nil)
}

val daterange = module("daterange")
val utils = module("utils", daterange)
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
val server = module("server", referencedatastore, marketdatastore, tradestore, xl)

val evcel = new Project(
  name = "evcel",
  root = new File("."),
  immediateUpstreamModules = List(server),
  props = makerProps
)
import evcel._
