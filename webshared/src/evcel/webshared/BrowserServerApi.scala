package evcel.webshared

object BrowserServerApi

//These are the only classes (+String/Int/...) which are sent through the browser<->server websocket

//All browser -> server messages are an EvcelRequest
sealed trait EvcelRequest
case class FragmentRequest(fragment:String) extends EvcelRequest
case class PageEvcelRequest(pageRequest: PageRequest) extends EvcelRequest

//All server -> browser messages are an EvcelResponse
sealed trait EvcelResponse
case class FragmentForPageResponse(forRequest:PageRequest, fragment:String) extends EvcelResponse
case class PageEvcelResponse(forRequest:PageRequest, pageResponse: PageResponse) extends EvcelResponse
case class PageRequestForFragmentResponse(pageRequest:PageRequest) extends EvcelResponse

//Each 'page' type has a request and response
sealed trait PageRequest
sealed trait PageResponse

case class HomePageRequest(i:Int) extends PageRequest
case class ReportPageRequest(parameters:WebRiskReportParameters, layout:WebPivotLayout) extends PageRequest
case class ListReferenceDataPivots(i:Int) extends PageRequest
case class ReferenceDataPivot(name:String, layout:WebPivotLayout) extends PageRequest

case class HomePageResponse(buttons:List[(String,PageRequest)]) extends PageResponse
case class ReportPageResponse(pivot:WebPivotResponse) extends PageResponse
case class ReferenceDataListPageResponse(buttons:List[(String,PageRequest)]) extends PageResponse
case class ReferenceDataPageResponse(pivot:WebPivotResponse) extends PageResponse

case class WebField(name:String, isMeasure:Boolean)
case class WebPivotResponse(spares:List[WebField], rowsWidth:Int, measuresWidth:Int, table:Seq[Seq[Cell]])
case class Cell(name:String, isHeading:Boolean = true, width:Int=1, height:Int=1)

case class WebDay(day:String)
case class WebCCY(name:String)
case class WebTenor(name:String)

case class WebRiskReportParameters(
  marketDay:WebDay,
  pricesCanMove:Boolean, fixingsShouldExist:Boolean,
  zeroIR:Boolean,
  showEqFutures: Boolean,
  baseCCY: WebCCY,
  valuationCcy: WebCCY,
  tenor: WebTenor) {
}
case class WebPivotArea(name:String)
object WebPivotArea {
  val Filter = WebPivotArea("filterarea")
  val Row = WebPivotArea("rowarea")
  val Column = WebPivotArea("columnarea")
  val Measure = WebPivotArea("measurearea")
}
case class WebPivotLayout(filters:List[String], rows:List[String], columns:List[String], measures:List[String]) {

  def transform(f:(WebPivotArea,List[String])=>List[String]) = {
    import WebPivotArea._
    copy(f(Filter, filters), f(Row, rows), f(Column, columns), f(Measure, measures))
  }
  def remove(field:String) = {
    transform( (_,fields) => fields.filter(_ != field))
  }
  def addTo(toArea:WebPivotArea, field:String, rightOf:Option[String]) = {
    def fixup(area:WebPivotArea, fields:List[String]) = {
      if (toArea != area) {
        fields.filter(_ != field)
      } else {
        rightOf match {
          case None => field :: fields.filter(_ != field)
          case Some(ro) => fields.flatMap { _ match {
            case `field` => List()
            case `ro` => List(ro, field)
            case f => List(f)
          } }
        }
      }
    }
    transform(fixup)
  }
  def addToEnd(toArea:WebPivotArea, field:String) = {
    def fixup(area:WebPivotArea, fields:List[String]) = {
      if (toArea != area) {
        fields.filter(_ != field)
      } else {
        fields.filter(_ != field) ::: List(field)
      }
    }
    transform(fixup)
  }

}


