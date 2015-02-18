package evcel.webjs

import evcel.webshared._
import org.scalajs.dom
import org.scalajs.dom.Element

import scala.scalajs.js
import scala.scalajs.js.{Function2, Object, Function}
import scala.scalajs.js.annotation.{JSName, JSExport}

import org.scalajs.jquery.jQuery

import js.JSConverters._

/**
 * Main entry point into the scalajs code
 */
object JSMain extends js.JSApp{

  @JSName("m")
  object Mithril extends js.Object {
    def module(element:Element, module:PageContext):Unit = js.native
  }

  def main() = {
    js.Dynamic.global.console.log("START")
    val atmosphereClient = new AtmosphereClient("/live")
    atmosphereClient.connect()
    val pageContext = new PageContext(atmosphereClient)
    val mainDiv = org.scalajs.dom.document.getElementById("main")
    Mithril.module(mainDiv, pageContext)
    val fragment = js.Dynamic.global.window.location.hash.asInstanceOf[String].stripPrefix("#")
    //If the user is using a bookmark there will be some fragment, otherwise we request the homepage
    if (fragment.startsWith("+")) {
      atmosphereClient.onNextConnect { () => atmosphereClient.send(FragmentRequest(fragment.stripPrefix("+"))) }
    } else {
      atmosphereClient.onNextConnect { () => atmosphereClient.send(FragmentRequest("")) }
      //not sure how to handle ? prefixed fragments
    }

  }
}


object PageRenderer {
  def redraw() {
    js.Dynamic.global.m.startComputation()
    js.Dynamic.global.m.endComputation()
  }
}