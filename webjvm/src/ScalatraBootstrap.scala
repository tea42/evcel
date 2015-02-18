import evcel.server.EvcelServer
import evcel.web._
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    val server = new EvcelServer
    val pageProcessor = new PageProcessor(server)
    context.mount(new MithrilServlet(), "/")
    context.mount(new LiveServlet(pageProcessor), "/live")
    context.mount(new CodeServlet, "/code")
  }
}
