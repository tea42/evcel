package evcel.web

import java.io.File

/**
 * Serves up the scalajs files
 */
class CodeServlet extends EvcellStack {

  get("/:module") {
    val filename = params("module")
    if (filename.endsWith(".map")) {
      val module = filename.stripSuffix("-fastopt.js.map")
      contentType = "application/octet-stream"
      new File(s"${module}/target/scala-2.11/${module}-fastopt.js.map")
    } else {
      val module = filename.stripSuffix(".js")
      contentType = " application/x-javascript"
      new File(s"${module}/target/scala-2.11/${module}-fastopt.js")
    }
  }

}
