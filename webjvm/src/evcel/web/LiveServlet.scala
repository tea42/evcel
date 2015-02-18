package evcel.web

// Default imports from a stock Scalatra g8 code generator:

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import evcel.webshared._
import org.apache.commons.codec.binary.Base64
import org.apache.commons.io.IOUtils
import org.atmosphere.cpr.BroadcasterFactory
import org.scalatra._
import org.scalatra.atmosphere._
import org.scalatra.json.{JValueResult, JacksonJsonSupport}
import org.json4s._

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class LiveServlet(pageProcessor:PageProcessor) extends EvcellStack with JValueResult
with JacksonJsonSupport with SessionSupport
with AtmosphereSupport {

  override protected implicit def wireFormat: WireFormat = {
    new WireFormat {
      def parseOutMessage(message: String): OutboundMessage = TextMessage(message)
      def parseInMessage(message: String): InboundMessage = TextMessage(message)
      def name: String = "text"
      def render(message: OutboundMessage): String = message.asInstanceOf[TextMessage].content
      def supportsAck: Boolean = false
    }
  }


  implicit protected val jsonFormats: Formats = DefaultFormats

  val connections = new ConcurrentHashMap[String,EvcelAtmosphereClient]()

  pageProcessor.addListener( (version:Long) => {
    if (BroadcasterFactory.getDefault != null) {
      import scala.collection.JavaConversions._
      connections.values().foreach( atmosphereClient => {
        atmosphereClient.sendUpdate()
      })
    }
  })

  private def decode(fragment:String):PageRequest = {
    if (fragment.isEmpty) {
      HomePageRequest(1)
    } else {
      val gzipInput = new GZIPInputStream(new ByteArrayInputStream(Base64.decodeBase64(fragment)))
      val json = IOUtils.toString(gzipInput, "UTF-8")
      upickle.read[PageRequest](json)
    }
  }

  private def encode(request:PageRequest):String = {
    if (request == HomePageRequest(1)) {
      return ""
    } else {
      val buffer = new ByteArrayOutputStream()
      val gzipOut = new GZIPOutputStream(buffer)
      gzipOut.write(upickle.write(request).getBytes("UTF-8"))
      gzipOut.close()
      Base64.encodeBase64String(buffer.toByteArray)
    }
  }

  class EvcelAtmosphereClient extends AtmosphereClient {
    scala.concurrent.ExecutionContext.Implicits.global
    var currentRequest:Option[PageRequest] = None
    def sendUpdate() {
      currentRequest.foreach(request => processAndSendPageRequest(request))
    }

    private def processAndSendPageRequest(pageRequest:PageRequest) {
      pageProcessor.pageResponseFor(pageRequest).onComplete {
        case Success(v) =>
          if (Some(pageRequest) == currentRequest) {
            sendResponse(PageEvcelResponse(pageRequest, v))
          }
        case Failure(e) =>
          e.printStackTrace() //TODO send exception to browser
      }
    }
    def receive: AtmoReceive = {
      case Connected =>
        println("Client %s is connected" format uuid)
        connections.put(uuid, this)
        //Don't try to send anything back to the client here. There is a scalatra bug.
        //A work around is to get the client to send an 'init' message and then respond to that.

      case Disconnected(ClientDisconnected, _) => { connections.remove(uuid) }
      case Disconnected(ServerDisconnected, _) => { connections.remove(uuid) }
      case t: TextMessage => {
        val request = upickle.read[EvcelRequest](t.content)
        request match {
          case pageRequest:PageEvcelRequest =>
            currentRequest = Some(pageRequest.pageRequest)
            sendResponse( FragmentForPageResponse(pageRequest.pageRequest, encode(pageRequest.pageRequest)) )
            processAndSendPageRequest(pageRequest.pageRequest)

          case FragmentRequest(fragment) =>
            val pageRequest = decode(fragment)
            currentRequest = Some(pageRequest)
            sendResponse( PageRequestForFragmentResponse(pageRequest) )
            processAndSendPageRequest(pageRequest)
        }
      }
      case JsonMessage(json) => println("ERROR don't expect native scalatra json: " + json)
    }
    private def sendResponse(response:EvcelResponse) {
      send(TextMessage(upickle.write(response)))
    }
  }
  atmosphere("/") {
    new EvcelAtmosphereClient
  }

  error {
    case t: Throwable => t.printStackTrace()
  }

  notFound {
    // remove content type in case it was set through an action
    contentType = null
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map { path =>
      contentType = "text/html"
      layoutTemplate(path)
    } orElse serveStaticResource() getOrElse resourceNotFound()
  }
}
