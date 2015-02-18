package evcel.webjs

import evcel.webshared._
import org.scalajs.dom

import scala.Dynamic
import scala.scalajs.js
import scala.scalajs.js.{Date, Dynamic}

/**
 *
 */
class ReportPivotPage(pageContext:PageContext) extends PivotPage(pageContext) with Page {

  type Request = ReportPageRequest
  type Response = ReportPageResponse


  override def pivotResponse(): Option[WebPivotResponse] = response.map(_.pivot)
  override def layout: WebPivotLayout = request.get.layout

  override def createRequest(c: WebPivotLayout): PageRequest = request.get.copy(layout = c)

  import MithrilElements._

  def form:js.Dynamic = {
    m("form#pivotform") {
      request match {
        case Some(r) =>
          val p = r.parameters
          ms("fieldset", Map()) {
            Seq(
              datePicker("marketday", "Market Day", p.marketDay.day),
              checkbox("pricescanmove", "Prices Can Move", p.pricesCanMove, (v:Boolean) => p.copy(pricesCanMove = v)),
              checkbox("fixingsshouldexist", "Fixings Should Exist", p.fixingsShouldExist, (v:Boolean) => p.copy(fixingsShouldExist = v)),
              checkbox("zeroir", "Zero IR", p.zeroIR, (v:Boolean) => p.copy(zeroIR = v)),
              checkbox("showeqfutures", "Show Eq Futures", p.showEqFutures, (v:Boolean) => p.copy(showEqFutures = v)),
              selectCCY("baseccy", "Base CCY", p.baseCCY.name, (v:String)=>p.copy(baseCCY = WebCCY(v))),
              selectCCY("valuationccy", "Valuation CCY", p.valuationCcy.name, (v:String)=>p.copy(valuationCcy = WebCCY(v))),
              select("tenor", "Tenor", p.tenor.name, (v:String) => p.copy(tenor = WebTenor(v)), List("Day", "Month", "Default"))
            )
          }
        case None =>
      }
    }
  }

  def changeParameter(p:WebRiskReportParameters) {
    pageContext.goTo(request.get.copy(parameters=p))
  }

  private def datePicker(name:String, label:String, value:String) = {
    def gotoDay(dayText:String) {
      changeParameter(request.get.parameters.copy(WebDay(dayText)))
    }
    def changeDay(offset:Int):js.Function = {
      (e:js.Dynamic) => {
        val newDay = new Date(request.get.parameters.marketDay.day)
        newDay.setDate(newDay.getDate()+offset)
        val text = f"${newDay.getUTCFullYear}%04d-${newDay.getMonth()+1}%02d-${newDay.getDate()}%02d"
        gotoDay(text)
        false
      }
    }
    def config(element:dom.html.Element, isInitialized:Boolean) {
      if (!isInitialized) {
        js.Dynamic.global.jQuery(element).datepicker(Dynamic.literal("dateFormat" -> "yy-mm-dd"))
      }
    }
    val onchange:js.Function = (e:js.Dynamic) => { gotoDay(e.target.value.asInstanceOf[String]) }
    ms("label", Map("for" -> name)) { Seq(
      me("input", Map("id" -> name, "name" -> name, "type" -> "text", "size" -> 10,
        "value" -> value, "config" -> ((config _):js.Function), "onchange" -> onchange)),
      mt("button.arrow", Map("onclick" -> changeDay(-1)), "<"),
      mt("button.arrow", Map("onclick" -> changeDay(+1)), ">"),
      label
    ) }
  }

  def checkbox(name:String, label:String, value:Boolean, f:(Boolean=>WebRiskReportParameters)) = {
    val onchange:js.Function = (e:js.Dynamic) => { changeParameter(f(e.target.checked.asInstanceOf[Boolean])) }
    ms("label", Map("for" -> name)) { Seq(
      me("input", Map("id" -> name, "name" -> name, "type" -> "checkbox", "checked" -> value, "onchange" -> onchange)), label)
    }
  }

  def selectCCY(name:String, label:String, value:String, f:(String=>WebRiskReportParameters)) = {
    select(name, label, value, f, List("USD", "GBP", "EUR"))
  }

  def select(name:String, label:String, value:String, f:(String=>WebRiskReportParameters), values:List[String]) = {
    val onchange:js.Function = (e:js.Dynamic) => changeParameter(f(e.target.value.asInstanceOf[String]))
    ms("label", Map("for" -> name)) { Seq(
      ms("select", Map("id" -> name, "name" -> name, "onchange" -> onchange)) {
        values.map(item => mt("option", Map("value" -> item, "selected" -> (item == value)), item))
      },
      label
    ) }
  }

}
