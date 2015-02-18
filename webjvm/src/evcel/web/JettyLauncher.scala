package evcel.web

import org.atmosphere.websocket.WebSocketHandler
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener

object JettyLauncher { // this is my entry object as specified in sbt project definition
  def main(args: Array[String]) {
    val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080

//  val wsHandler = new WebSocketHandler() {
//    def configure(factory:WebSocketServletFactory) {
//      //factory.register(MyEchoSocket.class);
//    }
//  }
//  ContextHandler context = new ContextHandler();
//  context.setContextPath("/echo");
//  context.setHandler(wsHandler);
//  server.addHandler(context);
//
//  ServerContainer wscontainer = WebSocketServerContainerInitializer.configureContext(context);
//
  val server = new Server(port)
    val context = new WebAppContext()
    context setContextPath "/"
    context.setResourceBase("webjvm/webapp")
    context.addEventListener(new ScalatraListener)
    context.addServlet(classOf[DefaultServlet], "/")

    server.setHandler(context)

    server.start
    server.join
  }
}