package evcel.webjs

import evcel.webshared._

import scala.scalajs.js

/**
 */
class ReferenceDataPage(pageContext:PageContext) extends PivotPage(pageContext) with Page {

  type Request = ReferenceDataPivot
  type Response = ReferenceDataPageResponse

  override def pivotResponse(): Option[WebPivotResponse] = response.map(_.pivot)
  override def layout: WebPivotLayout = request.get.layout

  override def createRequest(c: WebPivotLayout): PageRequest = request.get.copy(layout = c)

  import MithrilElements._

  def form:js.Dynamic = {
    mt("form", Map(), "")
  }
}
