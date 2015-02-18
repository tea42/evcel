package evcel.webjs

import evcel.webshared.{PageRequest, HomePageResponse}

import scala.scalajs.js
import scala.scalajs.js.Dynamic

/**
 * Just shows a list of buttons. Each button changes the page using the provided PageRequest
 */
abstract class ButtonsPage(pageContext:PageContext) extends Page {

  def buttons(r:Response):List[(String,PageRequest)]

  override def view(isReady:Boolean): Dynamic = {
    import MithrilElements._
    response match {
      case None => mt("p", Map(), "")
      case Some(r) =>
        ms("ul.buttons") {
          buttons(r).map { case (name, request) => {
            m("li") {
              mt("button", Map("onclick" -> ((() => pageContext.goTo(request)):js.Function)), name)
            }
          } }
        }
    }
  }
}
