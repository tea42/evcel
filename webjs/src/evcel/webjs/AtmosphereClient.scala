package evcel.webjs

import evcel.webshared.{EvcelResponse, EvcelRequest}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

/**
 * A small wrapper around the atmosphere javascript client
 */
class AtmosphereClient(url:String) {

  private var subSocket:Option[js.Dynamic] = None
  private var connected = false
  private val onConnectQueue = new ArrayBuffer[ ()=>Unit ]();
  private var handler:Option[(EvcelResponse)=>Unit] = None
  private val request = js.Dynamic.literal(
    "url" -> url,
    "contentType" -> "application/json",
    "logLevel" -> "debug",
    "transport" -> "websocket",
    "fallbackTransport" -> "long-polling",
    "reconnectInterval" -> 5000
  )

  request.onOpen = { (response:Any) =>
    connected = true
    // Seems there is a scalatra bug which prevents sending messages on connect so we send an empty message
    // from the client after connecting which the server then responds to with the initial report
    onConnectQueue.foreach(_())
    onConnectQueue.clear()
  }

  request.onReconnect = { (rq:Any, rs:Any) => { println("reconnect") } }
  request.onMessage = { (response:js.Dynamic) => handler.foreach { h =>
    val evcelResponse = upickle.read[EvcelResponse](response.responseBody.asInstanceOf[String])
    h(evcelResponse)
  } }
  request.onClose = { (rs:Any) => println("close"); connected = false }
  request.onError = { (rs:Any) => println("error") }

  def connect() {
    subSocket = Some(js.Dynamic.global.$.atmosphere.subscribe(request))
  }

  def onNextConnect(f: ()=>Unit) {
    onConnectQueue.append(f)
  }

  def onMessage(f: (EvcelResponse)=>Unit) {
    handler = Some(f)
  }

  def send(message:EvcelRequest) {
    subSocket match {
      case Some(s) if connected =>
        println("Sending: " + upickle.write(message))
        subSocket.get.push(upickle.write(message))
      case _ => throw new RuntimeException("Not connected")
    }
  }
}
