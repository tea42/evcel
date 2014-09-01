package com.evcel.server

import com.evcel.server.xlloop.XLLOopServer
import com.evcel.xl.EVPricing

object Main {
  def main(args: Array[String]) {
    val services = ("EVPricing", new EVPricing) :: Nil
    val xllServer = new XLLOopServer(1234, services)
    xllServer.start()
  }
}
