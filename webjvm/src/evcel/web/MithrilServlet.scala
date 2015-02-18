package evcel.web

/**
 * Serves the main root page (everything is scalajs after that)
 */
class MithrilServlet extends EvcellStack {

  get("/") {
    contentType = "text/html"
    jade("mithril",
      "title" -> "Pivot",
      "javascript" -> "startEvcel()")
  }
}
