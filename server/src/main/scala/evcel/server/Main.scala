package evcel.server

import evcel.server.xlloop.XLLOopServer
import evcel.xl.EVPricing

object Main {
  def main(args: Array[String]) {
    val services = ("EVPricing", new EVPricing) :: Nil
    val xllServer = new XLLOopServer(1234, services)
    xllServer.start()
  }
}
