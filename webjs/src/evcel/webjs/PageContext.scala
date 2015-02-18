package evcel.webjs

import evcel.webshared._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import upickle._

trait Page {
  type Request
  type Response
  var request:Option[Request] = None
  var response:Option[Response] = None
  def view(isReady:Boolean):js.Dynamic
}

/**
 * Messy class which manages the different states the page can be in.
 * For example, waiting for the fragment for the page just requested, waiting for the page response,
 *  sending a new fragment to the server because the user pressed back
 */
class PageContext(atmosphereClient: AtmosphereClient) {

  private var temporaryFragments = Map[String,PageRequest]()
  private var seq = 0
  def temporaryFragmentFor(request:PageRequest) = {
    val fragment = seq.toString
    seq += 1
    temporaryFragments = temporaryFragments + (fragment -> request)
    fragment
  }

  var waitingForResponse = false
  var pageRequest:Option[PageRequest] = None
  var page:Option[Page] = None
  var lastLocationHash = ""

  atmosphereClient.onMessage( (response:EvcelResponse) => {
    response match {
      case pageResponse: PageEvcelResponse=>
        if (pageRequest == Some(pageResponse.forRequest)) {
          waitingForResponse = false
          page.foreach(p => p.response = Some(pageResponse.pageResponse.asInstanceOf[p.Response]))
          PageRenderer.redraw()
        }
      case FragmentForPageResponse(forRequest, fragment) =>
        if (pageRequest == Some(forRequest)) {
          lastLocationHash = "+" + fragment
          js.Dynamic.global.window.location.replace("#+" + fragment)
        }
      case PageRequestForFragmentResponse(pageRequest) =>
        showPage(pageRequest)
    }
  } )
  js.Dynamic.global.window.onhashchange = () => {
    val fragment = js.Dynamic.global.window.location.hash.asInstanceOf[String].stripPrefix("#")
    if (fragment != lastLocationHash) {
      lastLocationHash = fragment
      if (fragment.isEmpty) {
        waitingForResponse = true
        atmosphereClient.send(FragmentRequest(""))
      } else {
        fragment.charAt(0) match {
          case '+' =>
            waitingForResponse = true
            atmosphereClient.send(FragmentRequest(fragment.substring(1)))
          case '?' =>
            val request = temporaryFragments(fragment.substring(1))
            waitingForResponse = true
            atmosphereClient.send(PageEvcelRequest(request))
            showPage(request)
          case o => println("Unexpected fragment prefix: " + o + " in " + fragment)
        }
      }
    }
  }

  def goTo(request:PageRequest) {
    val temporaryFragment = temporaryFragmentFor(request)
    lastLocationHash = "?" + temporaryFragment
    js.Dynamic.global.window.location.hash = "?" + temporaryFragment
    waitingForResponse = true
    atmosphereClient.send(PageEvcelRequest(request))
    showPage(request)
  }

  private def showPage(request:PageRequest) {
    if (pageRequest.isEmpty || pageRequest.get.getClass != request.getClass) {
      page = Some(pageFor(request))
    }
    page match {
      case Some(p) => p.request = Some(request.asInstanceOf[p.Request])
      case _ =>
    }
    pageRequest = Some(request)
    PageRenderer.redraw()
  }

  @JSExport def view() = {
    page match {
      case None =>
      case Some(p) => p.view(!waitingForResponse)
    }
  }

  def pageFor(pageRequest: PageRequest):Page = {
    pageRequest match {
      case _:HomePageRequest => new HomePage(this)
      case _:ReportPageRequest => new ReportPivotPage(this)
      case _:ListReferenceDataPivots => new ReferenceDataButtonPage(this)
      case _:ReferenceDataPivot => new ReferenceDataPage(this)
    }
  }

}
