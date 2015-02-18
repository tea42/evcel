package evcel.webjs

import evcel.webshared.{ListReferenceDataPivots, ReferenceDataListPageResponse, PageRequest}

/**
 * Lists the types of reference data available
 */
class ReferenceDataButtonPage(pageContext: PageContext) extends ButtonsPage(pageContext) {
  override type Request = ListReferenceDataPivots
  override type Response = ReferenceDataListPageResponse
  override def buttons(r: ReferenceDataListPageResponse): List[(String, PageRequest)] = r.buttons

}
