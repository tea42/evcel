package com.evcel.server.xlloop

import org.boris.xlloop.FunctionServer
import org.boris.xlloop.handler.{ CompositeFunctionHandler, FunctionInformationHandler }
import org.boris.xlloop.reflect.ReflectFunctionHandler

class XLLOopServer(port: Int, export: List[(String, Object)]) {

  def start() {
    val functions = new ReflectFunctionHandler()
    export.foreach { case (name, obj) => functions.addMethods(name + ".", obj) }
    val info = new FunctionInformationHandler()
    info.add(functions.getFunctions)

    val server = new FunctionServer(port)
    val functionsAndInfo = new CompositeFunctionHandler()
    functionsAndInfo.add(functions)
    functionsAndInfo.add(info)
    server.setFunctionHandler(functionsAndInfo)
    println("XLLOopServer started on port: " + port)
    server.run()
  }
}
