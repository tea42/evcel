package evcel.webjs

import evcel.webshared.{PageRequest, HomePageRequest, HomePageResponse}

import scala.scalajs.js.Dynamic

/**
 *
 */
class HomePage(pageContext:PageContext) extends ButtonsPage(pageContext) {
  type Request = HomePageRequest
  type Response = HomePageResponse

  override def buttons(r: HomePageResponse): List[(String, PageRequest)] = r.buttons

}
