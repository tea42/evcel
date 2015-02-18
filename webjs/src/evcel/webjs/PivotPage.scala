package evcel.webjs

import evcel.webshared._
import org.scalajs.dom
import org.scalajs.dom.Element

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport}

import org.scalajs.jquery.jQuery


abstract class PivotPage(pageContext:PageContext) extends Page {

  import MithrilElements._

  def pivotResponse():Option[WebPivotResponse]
  def createRequest(c:WebPivotLayout):PageRequest
  def layout:WebPivotLayout

  def view(isReady:Boolean) = {
    ms("div") {
      Seq(form, table(isReady))
    }
  }

  def form:Dynamic

  trait DropTarget
  case object Remove extends DropTarget
  case object NoChange extends DropTarget
  case class MoveTo(area:WebPivotArea, rightOf:Option[String] /*None means first item*/) extends DropTarget

  var dragged:Option[String] = None //If a field is being dragged right no this is the name of the field
  var dropTarget:DropTarget = NoChange //The current target for the dragged field
  var selectedCell:Option[String] = None //The currently selected (last clicked) cell
  var filterText:String = "" //The filter text for reducing the fields shown on the left

  def dragEnd(e:js.Dynamic) {
    (dragged,dropTarget) match {
      case (Some(field), MoveTo(area,rightOf)) => pageContext.goTo(createRequest(layout.addTo(area, field, rightOf)))
      case (Some(field), Remove) => pageContext.goTo(createRequest(layout.remove(field)))
      case _ =>
    }
    dragged = None
    dropTarget = NoChange
  }
  def dragStart(e:js.Dynamic) {
    e.dataTransfer.effectAllowed = "move"
    e.dataTransfer.setData("text/html", null)
    dragged = Some(e.currentTarget.dataset.field.asInstanceOf[String])
  }
  def isTarget(fieldArea:WebPivotArea, field:String) = dropTarget == MoveTo(fieldArea,Some(field))

  def fieldArea(fieldArea:WebPivotArea, fields:List[String]) = {
    def marker(show:Boolean) = mt("div",Map("class"->("marker " + (if (show) "marked" else ""))), ".")
    def fieldLI(field:String) = {
      val attributes = Map("id" -> field, "key" -> field, "data-field" -> field, "draggable" -> "true",
        "ondragstart" -> f(dragStart _),
        "ondragend" -> f(dragEnd _),
        "ondblclick" -> f(doubleClick _)
      )
      List( mt("li", attributes, field), marker(isTarget(fieldArea,field)) )
    }
    def doubleClick(e:js.Dynamic) = {
      var field = e.currentTarget.asInstanceOf[dom.Element].getAttribute("id")
      pageContext.goTo(createRequest(layout.remove(field)))
    }
    def onOver(e:js.Dynamic) {
      e.preventDefault()
      var ul = e.currentTarget.asInstanceOf[dom.Element]
      var rightOf:Option[String] = None
      var leftOf:Option[String] = None
      val lis = jQuery(ul).children("li")
      for (i <- (0 until lis.length)) {
        val li = jQuery(lis.get(i))
        val middle = li.offset().asInstanceOf[js.Dynamic].left + (li.width() / 2).asInstanceOf[js.Dynamic]
        if ( (e.clientX > middle).asInstanceOf[Boolean]) {
          rightOf = Some(li.attr("id"))
          if ((i+1) < lis.length) {
            leftOf = Some(jQuery(lis.get(i+1)).attr("id"))
          }
        }
      }
      dropTarget = if (dragged==rightOf || dragged==leftOf) NoChange else MoveTo(fieldArea, rightOf)
    }
    def onLeave(e:js.Dynamic) {
      dropTarget = Remove
    }
    ms("ul", Map("class" -> "area", "ondragover" -> f(onOver _), "ondragleave" -> f(onLeave _))) {
      marker(dropTarget == MoveTo(fieldArea,None)) :: fields.flatMap(fieldLI)
    }
  }

  def fieldList() = {
    val spares = pivotResponse().map(_.spares).getOrElse(Nil)
    val fields = {
      val normalizedText = filterText.trim.toLowerCase
      val allFields = spares.map(_.name)
      if (normalizedText.isEmpty) allFields else allFields.filter(_.toLowerCase.contains(normalizedText))
    }
    def fieldLI(field:String) = {
      val attributes = Map("id" -> field, "key" -> field, "data-field" -> field, "draggable" -> "true",
        "ondragstart" -> f(dragStart _),
        "ondragend" -> f(dragEnd _),
        "ondblclick" -> f(doubleClick _)
      )
      mt("li", attributes, field)
    }
    def doubleClick(e:js.Dynamic) = {
      var field = e.currentTarget.asInstanceOf[dom.Element].getAttribute("id")
      spares.find(_.name == field) match {
        case Some(WebField(_, isMeasure)) =>
          val area = if (isMeasure) WebPivotArea.Measure else WebPivotArea.Row
          pageContext.goTo(createRequest(layout.addToEnd(area, field)))
        case _ =>
      }
    }
    def setFilterText(e:js.Dynamic) {
      filterText = e.target.value.asInstanceOf[String]
    }
    ms("ul", Map("id" -> "fieldarea", "class" -> "area")) {
      me("input", Map("name" -> "fieldsfilter", "value"->filterText,
        "type" -> "text", "size" -> 10, "oninput" -> f(setFilterText _) )) :: fields.map(fieldLI)
    }
  }

  private def table(isReady:Boolean) = {
    def tableClick(e:js.Dynamic) {
      selectedCell = Some(e.toElement.asInstanceOf[dom.Element].getAttribute("id"))
    }
    pivotResponse match {
      case None => mt("div", Map(), "")
      case Some(p) =>
        var id = 1
        ms("div#mainTable") {
          Seq(
            fieldList(),
            fieldArea(WebPivotArea.Filter, layout.filters),
            ms("table", Map("onmousedown" -> f(tableClick))) {
              Seq(
                ms("tr") {
                  Seq(ms("td", Map("colspan" -> p.rowsWidth)) {
                    Seq()
                  },
                    m("td", Map("class" -> "fieldareacell", "colspan" -> p.measuresWidth)) {
                      fieldArea(WebPivotArea.Column, layout.columns)
                    })
                },
                ms("tr") {
                  Seq(m("td", Map("class" -> "fieldareacell", "id" -> "rowareacell", "colspan" -> p.rowsWidth)) {
                    fieldArea(WebPivotArea.Row, layout.rows)
                  },
                    m("td", Map("class" -> "fieldareacell", "colspan" -> p.measuresWidth)) {
                      fieldArea(WebPivotArea.Measure, layout.measures)
                    })
                }) ++
                p.table.map { row =>
                  ms("tr", if (isReady) Map() else Map("class" -> "working")) {
                    row.map { cell =>
                      val config:js.Function = (element:js.Dynamic, isInitialized:Boolean, context:js.Dynamic) => {
                        if (!isInitialized) {
                          context.text = element.textContent
                        } else {
                          val newText = element.textContent
                          if (!cell.isHeading && context.text != newText) {
                            val jQueryElement = js.Dynamic.global.jQuery(element)
                            jQueryElement.css(js.Dynamic.literal("backgroundColor" -> "#fffff0"))
                            jQueryElement.velocity(
                              js.Dynamic.literal("backgroundColor" -> "#aad4ff"),
                              js.Dynamic.literal("duration" -> 2000))
                            context.text = element.textContent
                          }
                        }
                      }
                      val myId = s"td${ id+=1; id }"
                      val style = if (Some(myId) == selectedCell) "background-color: red" else ""
                      ms(if (cell.isHeading) "th" else "td",
                        Map("id" -> myId, "style" -> style,
                          "colspan" -> cell.width, "rowspan" -> cell.height, "config" -> config)) { Seq(cell.name) }
                    }
                  }
                }
            }
          )
        }
    }
  }
}
