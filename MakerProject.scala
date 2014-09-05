import maker.project.{Module, Project}
import java.io.File
import scala.util.Properties
import maker.task.BuildResult
import maker.task.Dependency
import maker.utils.os.Command
import maker.utils.os.CommandOutputHandler

Properties.setProp("scala.usejavacp", "false")
Properties.setProp("log4j.ignoreTCL", "true")
Properties.clearProp("scala.home")

def module(name : String, upstream : Module*) = {
  new Module(root = new File(name), name = name, immediateUpstreamModules = upstream.toList)
}

val maths = module("maths")
val daterange = module("daterange", maths)
val quantity = module("quantity", maths)
val curve = module("curve", quantity, daterange, maths)
val eventstore = module("eventstore", curve)
val calendar = module("calendar", daterange)
val calendarstore = module("calendarstore", calendar, eventstore)
val evcel = new Project(
  name = "evcel",
  root = new File("."),
  immediateUpstreamModules = List(calendarstore)
){
  def launchTestKafka() = {
    Command("bin/start.sh", "-t").exec == 0
  }
  def shutdownKafka() = {
    Command("bin/stop.sh").exec == 0
  }
  override def setUp(graph : Dependency.Graph) = {
    var result = super.setUp(graph)

    if (result && graph.includesTestTask)
      result = launchTestKafka()
    result
  }
  override def tearDown(graph : Dependency.Graph, buildResult : BuildResult) = {
    val kafkaResult = if (graph.includesTestTask)
      shutdownKafka()
    else
      true
    // Always call super.testDown, even if kafka shutdown fails
    super.tearDown(graph, buildResult) && kafkaResult
  }
}

import evcel._
